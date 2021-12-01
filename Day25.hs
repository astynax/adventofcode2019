#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (chr, ord)

import qualified Input
import Errors
import Intcode

newtype IEnv m a = IEnv { runIEnv :: m a }
  deriving (Functor, Applicative, Monad)

instance
    ( MonadIO m
    , MonadState String m
    ) => MonadEnv (IEnv m) where
  envInput       = IEnv go
    where
      go = get >>= \case
        []     -> liftIO getLine >>= (put . expand . (++ "\n")) >> go
        (x:xs) -> ord x <$ put xs
  envOutput v    = IEnv $ liftIO $ putChar (chr v)
  envTrace _ _ _ = pure Continue

main :: IO ()
main = do
  program <- Input.intsFile "Day25.input"
  runInteractive program

runInteractive :: MonadIO m => Intcode -> m ()
runInteractive program =
  runIEnv (Intcode.runIntcode program) `evalStateT` "" >>= \case
    (Left err, _) -> failWith $ "Intcode error: " ++ show err
    (_, _)        -> pure ()

expand :: String -> String
expand []           = ""
expand s@(c:_)
  | c `elem` "nsew" = unlines $ filter (not . null) $ map fromChar s
  | otherwise       = s
  where
    fromChar 'n' = "north"
    fromChar 's' = "south"
    fromChar 'e' = "east"
    fromChar 'w' = "west"
    fromChar _   = ""
