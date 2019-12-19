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

newtype ScannerEnv a = SEnv (StateT Scanner IO a)
  deriving (Functor, Applicative, Monad)

data Scanner = Scanner
  { sInput  :: ![Int]
  , sOutput :: !(Maybe Int)
  }

instance MonadEnv ScannerEnv where
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
  print "TODO"

runScannerEnv :: ScannerEnv a -> Pos -> IO (a, Maybe Int)
runScannerEnv (SEnv a) (x, y) =
  fmap sOutput <$> a `runStateT` Scanner
    { sInput = [x, y]
    , sOutput = Nothing
    }

scanPos :: Intcode -> Pos -> IO Bool
scanPos program pos =
  runScannerEnv (Intcode.runIntcode program) pos >>= \case
    ((Left err, _), _)       -> failWith $ show err
    (_,             Nothing) -> failWith $ "No output for " ++ show pos
    (_,             Just x)  -> pure $ x == 1

traceBeam :: Intcode -> IO (AreaMap ())
traceBeam program = foldM step Map.empty positions
  where
    step m pos =
      scanPos program pos >>= \case
        True -> pure $ Map.insert pos () m
        _    -> pure m
    positions =
      [ (x, y)
      | y <- [0..9]
      , x <- [0..9]
      ]
