#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=vector,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Vector as V

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import qualified Input
import Intcode

main :: IO ()
main = do
  state <- IntcodeState 0 . V.fromList <$> Input.ints
  putStrLn "Step 1"
  runWithId state 1
  putStrLn "Step 2"
  runWithId state 5
  where
    runWithId s r =
      runReaderT (Intcode.run s eval) r >>= \case
        (Left err, st) -> putStrLn err >> print st
        _              -> pure ()

eval :: (MonadIO m, MonadReader Int m, MonadIntcode m) => m ()
eval = do
  op <- readOp
  unless (op == Stop) $ do
    case op of
      Stop        -> pure ()
      In        a -> ask >>= push a
      Out p       -> getParam p >>= liftIO . print
      Jnz p1 p2   -> jumpIf (/= 0) p1 p2
      Jz  p1 p2   -> jumpIf (== 0) p1 p2
      Add p1 p2 a -> binOp (+)     p1 p2 a id
      Mul p1 p2 a -> binOp (*)     p1 p2 a id
      Lt  p1 p2 a -> binOp (<)     p1 p2 a fromBool
      Eq  p1 p2 a -> binOp (==)    p1 p2 a fromBool
    eval
  where
    getParam (Position a)  = load a
    getParam (Immediate x) = pure x
    fromBool True = 1
    fromBool _    = 0
    binOp op p1 p2 a f =
      liftA2 op (getParam p1) (getParam p2) >>= push a . f
    jumpIf cond p1 p2 = do
      v <- getParam p1
      a <- getParam p2
      when (cond v) $ jump $ Addr a
