#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import Area
import qualified Input
import Errors
import Intcode

newtype ScannerEnv m a = SEnv (StateT Scanner m a)
  deriving (Functor, Applicative, Monad)

data Scanner = Scanner
  { sInput  :: ![Int]
  , sOutput :: !(Maybe Int)
  }

instance MonadIO m => MonadEnv (ScannerEnv m) where
  envInput     = SEnv $
    gets sInput >>= \case
      []     -> failWith "Input underflow!"
      (i:is) -> i <$ modify (\s -> s { sInput = is })
  envOutput v  = SEnv $ modify $ \s -> s { sOutput = Just v }
  envTrace _ _ = \case
    Out _ -> SEnv $ pure StopAfter
    _     -> SEnv $ pure Continue

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  m <- traceBeam program
  flip Area.visualize m $ \case
    Just () -> '#'
    Nothing -> '.'
  print $ length m
  putStrLn "Step 2"
  (x, y) <- searchPlace program 900
  print $ x * 10000 + y

runScannerEnv :: MonadIO m => ScannerEnv m a -> Pos -> m (a, Maybe Int)
runScannerEnv (SEnv a) (x, y) =
  fmap sOutput <$> a `runStateT` Scanner
    { sInput = [x, y]
    , sOutput = Nothing
    }

scanPos :: MonadIO m => Intcode -> Pos -> m Bool
scanPos program pos =
  runScannerEnv (Intcode.runIntcode program) pos >>= \case
    ((Left err, _), _)       -> failWith $ show err
    (_,             Nothing) -> failWith $ "No output for " ++ show pos
    (_,             Just x)  -> pure $ x == 1

traceBeam :: MonadIO m => Intcode -> m (AreaMap ())
traceBeam program = foldM step Map.empty positions
  where
    step m pos =
      scanPos program pos >>= \case
        True -> pure $ Map.insert pos () m
        _    -> pure m
    positions =
      [ (x, y)
      | y <- [0..49]
      , x <- [0..49]
      ]

searchPlace :: Intcode -> Int -> IO Pos
searchPlace prog offset =
  (start >> go) `runReaderT` prog `evalStateT` (0, offset)
  where
    start = beam R not >> step R
    go = do
      (x, y) <- get
      here <- scan
      unless here $ failWith $ show (x, y)
      opp <- join $ asks scanPos <*> pure (x + 99, y - 99)
      if opp
        then pure (x, y - 99)
        else do
          step R >> beam D id
          go
    step = modify . move
    scan = join $ asks scanPos <*> get
    beam dir test = do
      old <- get
      step dir
      test <$> scan >>= \case
        True -> beam dir test
        _    -> put old
