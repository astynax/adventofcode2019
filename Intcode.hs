{- stack --package=megaparsec,vector,mtl -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Intcode
  ( Addr(..)
  , Param(..)
  , Op(..)
  , IntcodeState(..)
  , MonadIntcode
  , readOp, load, push, jump
  , run
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Vector (Vector, (//), (!?))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data ParamMode
  = MPosition
  | MImmediate
  deriving Show

data RawOp = RawOp ParamMode ParamMode ParamMode Int
  deriving Show

newtype Addr = Addr Int deriving (Show, Eq)

data Param
  = Position Addr
  | Immediate Int
  deriving (Show, Eq)

data Op
  = Add Param Param Addr
  | Mul Param Param Addr
  | In              Addr
  | Out Param
  | Jnz Param Param
  | Jz  Param Param
  | Lt  Param Param Addr
  | Eq  Param Param Addr
  | Stop
  deriving (Show, Eq)

data IntcodeState = IntcodeState
  { isPC     :: Int
  , isMemory :: Vector Int
  }
  deriving Show

type MonadIntcode m = (MonadState IntcodeState m, MonadError String m)

readOp :: MonadIntcode m => m Op
readOp = do
  rawOpValue <- pull
  rawOp <- case parse rawOpP "" (reverse $ show rawOpValue) of
    Right x  -> pure x
    Left err -> throwError $ errorBundlePretty err
  case rawOp of
    RawOp a b MPosition 1  ->
      Add <$> readParam a <*> readParam b <*> readAddr
    RawOp a b MPosition 2  ->
      Mul <$> readParam a <*> readParam b <*> readAddr
    RawOp _ _ MPosition 3  ->
      In  <$> readAddr
    RawOp a _ _         4  ->
      Out <$> readParam a
    RawOp a b _         5  ->
      Jnz <$> readParam a <*> readParam b
    RawOp a b _         6  ->
      Jz  <$> readParam a <*> readParam b
    RawOp a b MPosition 7  ->
      Lt  <$> readParam a <*> readParam b <*> readAddr
    RawOp a b MPosition 8  ->
      Eq  <$> readParam a <*> readParam b <*> readAddr
    RawOp _ _ _         99 ->
      pure Stop
    _                      ->
      throwError $ "Bad op: " ++ show rawOpValue
  where
    readParam MImmediate = Immediate <$> pull
    readParam MPosition  = Position  <$> readAddr
    readAddr = pull >>= \case
      x | x >= 0 -> pure $ Addr x
      x          -> throwError $ "Bad addr: " ++ show x

pull :: MonadIntcode m => m Int
pull = do
  IntcodeState pos mem <- get
  cell <- case mem !? pos of
    Just x  -> pure x
    Nothing -> throwError "PC is out of bounds!"
  put $ IntcodeState (pos + 1) mem
  pure cell

load :: MonadIntcode m => Addr -> m Int
load (Addr a) =
  gets isMemory >>= \mem -> case mem !? a of
    Just x  -> pure x
    Nothing -> throwError "Addr is out of bounds!"

push :: MonadIntcode m => Addr -> Int -> m ()
push (Addr a) v = modify $ \c -> c
  { isMemory = isMemory c // [(a, v)]
  }

jump :: MonadIntcode m => Addr -> m ()
jump (Addr a) = modify $ \c -> c { isPC = a }

rawOpP :: Parsec Void String RawOp
rawOpP = do
  e <- digitP
  op <- optional digitP >>= \case
    Just d ->
      RawOp <$> modeP <*> modeP <*> modeP <*> pure (d * 10 + e)
    Nothing ->
      pure $ RawOp MPosition MPosition MPosition e
  eof
  pure op
  where
    digitP = digitToInt <$> digitChar
    modeP = try (
      digitChar >>= \case
         '0' -> pure MPosition
         '1' -> pure MImmediate
         _   -> fail "Bad mode!"
      ) <|> pure MPosition

run
  :: Monad m
  => IntcodeState
  -> ExceptT String (StateT IntcodeState m) ()
  -> m (Either String (), IntcodeState)
run s a = runStateT (runExceptT a) s
