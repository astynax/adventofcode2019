{- stack --package=megaparsec,vector,mtl -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Intcode
  ( Addr(..)
  , Param(..)
  , Op(..)
  , IntcodeState(..)
  , MonadIntcode
  , MonadEnv(..)
  , SimpleEnv()
  , Control(..)
  , runSimpleEnv
  , readOp, load, push, jump
  , initState, runIntcode, rerunIntcode
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Vector (Vector, (//), (!?))
import qualified Data.Vector as V
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
  | Stp
  deriving (Show, Eq)

data IntcodeState = IntcodeState
  { isPC     :: Addr
  , isMemory :: Vector Int
  }
  deriving Show

type MonadIntcode m =
  ( MonadState IntcodeState m
  , MonadError String m
  )

data Control
  = Continue
  | StopBefore
  | StopAfter
  deriving (Show, Eq)

class Monad m => MonadEnv m where
  envInput  :: m Int
  envOutput :: Int -> m ()
  envTrace  :: IntcodeState -> Addr -> Op -> m Control

instance MonadEnv m => MonadEnv (StateT s m) where
  envInput = lift envInput
  envOutput = lift . envOutput
  envTrace s a o = lift $ envTrace s a o

instance MonadEnv m => MonadEnv (ExceptT e m) where
  envInput = lift envInput
  envOutput = lift . envOutput
  envTrace s a o = lift $ envTrace s a o

newtype SimpleEnv m a = SimpleEnv (ReaderT Int m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadEnv (SimpleEnv m) where
  envInput = SimpleEnv ask
  envOutput x = SimpleEnv (liftIO $ print x)
  envTrace _ _ _ = SimpleEnv $ pure Continue

runSimpleEnv :: MonadIO m => SimpleEnv m a -> Int -> m a
runSimpleEnv (SimpleEnv r) = runReaderT r

initState :: [Int] -> IntcodeState
initState = IntcodeState (Addr 0) . V.fromList

runIntcode
  :: MonadEnv m
  => [Int]
  -> m (Either String (), IntcodeState)
runIntcode = rerunIntcode . initState

rerunIntcode
  :: MonadEnv m
  => IntcodeState
  -> m (Either String (), IntcodeState)
rerunIntcode = runStateT (runExceptT eval)

eval :: (MonadEnv m, MonadIntcode m) => m ()
eval = do
  (addr, op) <- readOp
  control <- get >>= \s -> envTrace s addr op
  if control == StopBefore
  then modify (\s -> s { isPC = addr})  -- restore PC
  else unless (op == Stp) $ do
    case op of
      Stp         -> pure ()
      Jnz p1 p2   -> jumpIf (/= 0) p1 p2
      Jz  p1 p2   -> jumpIf (== 0) p1 p2
      Add p1 p2 a -> binOp (+)     p1 p2 a id
      Mul p1 p2 a -> binOp (*)     p1 p2 a id
      Lt  p1 p2 a -> binOp (<)     p1 p2 a fromBool
      Eq  p1 p2 a -> binOp (==)    p1 p2 a fromBool
      In        a -> envInput >>= push a
      Out p       -> getParam p >>= envOutput
    when (control == Continue) eval
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

readOp :: MonadIntcode m => m (Addr, Op)
readOp = do
  start <- gets isPC
  rawOpValue <- pull
  rawOp <- case parse rawOpP "" (reverse $ show rawOpValue) of
    Right x  -> pure x
    Left err -> throwError $ errorBundlePretty err
  (start,) <$> case rawOp of
    RawOp a b MPosition 1  -> Add <$> p a <*> p b <*> addr
    RawOp a b MPosition 2  -> Mul <$> p a <*> p b <*> addr
    RawOp _ _ MPosition 3  -> In  <$>                 addr
    RawOp a _ _         4  -> Out <$> p a
    RawOp a b _         5  -> Jnz <$> p a <*> p b
    RawOp a b _         6  -> Jz  <$> p a <*> p b
    RawOp a b MPosition 7  -> Lt  <$> p a <*> p b <*> addr
    RawOp a b MPosition 8  -> Eq  <$> p a <*> p b <*> addr
    RawOp _ _ _         99 -> pure Stp
    _                      ->
      throwError $ "Bad op: " ++ show rawOpValue
  where
    p MImmediate = Immediate <$> pull
    p MPosition  = Position  <$> addr
    addr = pull >>= \case
      x | x >= 0 -> pure $ Addr x
      x          -> throwError $ "Bad addr: " ++ show x

pull :: MonadIntcode m => m Int
pull = do
  IntcodeState (Addr pos) mem <- get
  cell <- case mem !? pos of
    Just x  -> pure x
    Nothing -> throwError "PC is out of bounds!"
  put $ IntcodeState (Addr $ pos + 1) mem
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
jump a = modify $ \c -> c { isPC = a }

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
