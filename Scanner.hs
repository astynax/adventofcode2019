#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.List
import qualified Data.Map.Strict as Map

import Area
import qualified Input
import Errors
import Intcode

newtype ScannerEnv a = SEnv (StateT Scanner IO a)
  deriving (Functor, Applicative, Monad)

data Scanner = Scanner
  { sInput    :: ![Int]
  , sLastPos  :: !(Maybe (Int, Maybe Int))
  , sMap      :: !(AreaMap ())
  }

instance MonadEnv ScannerEnv where
  envInput       = SEnv $
    gets sInput >>= \case
      []     -> failWith "Input underflow!"
      (i:is) -> do
        lp <- gets sLastPos >>= \case
          Nothing           -> pure $ Just (i, Nothing)
          Just (x, Nothing) -> pure $ Just (x, Just i)
          _                 -> failWith "No coresponding output!"
        modify $ \s -> s
          { sInput   = is
          , sLastPos = lp
          }
        pure i
  envOutput v    = SEnv $
    gets sLastPos >>= \case
      Just (x, Just y) -> modify $ \s@Scanner{sMap} -> s
        { sMap     = (if v /= 0 then Map.insert (x, y) () else id) sMap
        , sLastPos = Nothing
        }
      _                -> failWith "No corresponding input!"
  envTrace _ _ _ = SEnv $ pure Continue

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  m <- runScan program
  flip Area.visualize m $ \case
    Just () -> '#'
    Nothing -> '.'

runScannerEnv :: ScannerEnv a -> IO (a, AreaMap ())
runScannerEnv (SEnv a) = fmap sMap <$> a `runStateT` Scanner
  { sLastPos = Nothing
  , sMap     = Map.empty
  , sInput   = concat
    [ [x, y]
    | y <- [0..49]
    , x <- [0..49]
    ]
  }

runScan :: Intcode -> IO (AreaMap ())
runScan program =
  runScannerEnv (Intcode.runIntcode program) >>= \case
    ((Left err, _), _) -> failWith $ show err
    (_,             x) -> pure x
