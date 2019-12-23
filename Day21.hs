#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char

import qualified Input
import Errors
import Intcode

data RobotState = RobotState
  { rsInput  :: String
  , rsValue  :: Maybe Int
  }

newtype RobotEnv m a = RobotEnv (StateT RobotState m a)
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadEnv (RobotEnv m) where
  envInput       = RobotEnv $
    gets rsInput >>= \case
      (x:xs) -> ord x <$ modify (\s -> s { rsInput = xs })
      _      -> failWith "Input buffer underflow!"
  envOutput v    = RobotEnv $
    if v >= 256
    then modify (\s -> s { rsValue  = Just v })
    else liftIO (putChar $ chr v)
  envTrace _ _ _ = RobotEnv $ pure Continue

type SpringScript m = [SpringOp m]

data RegMode = R | RW

data RunMode = Run | Walk

data Reg (m :: RunMode) (r :: RegMode) where
  A :: Reg m    'R
  B :: Reg m    'R
  C :: Reg m    'R
  D :: Reg m    'R
  E :: Reg 'Run 'R
  F :: Reg 'Run 'R
  G :: Reg 'Run 'R
  H :: Reg 'Run 'R
  I :: Reg 'Run 'R
  T :: Reg m    'RW
  J :: Reg m    'RW

deriving instance Show (Reg m r)

data SpringOp (m :: RunMode) where
  OR  :: forall r m. Reg m r -> Reg m 'RW -> SpringOp m
  AND :: forall r m. Reg m r -> Reg m 'RW -> SpringOp m
  NOT :: forall r m. Reg m r -> Reg m 'RW -> SpringOp m

deriving instance Show (SpringOp 'Run)
deriving instance Show (SpringOp 'Walk)

data ModeName (m :: RunMode) = MN

instance Show (ModeName 'Walk) where
  show _ = "WALK"

instance Show (ModeName 'Run) where
  show _ = "RUN"

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  print =<< runRobot @'Walk program
    [ NOT C J
    , AND D J
    , NOT A T
    , OR  T J
    ]
  putStrLn "Step 2"
  print =<< runRobot @'Run program
    -- @ABCDEFGHI
    -- #???!~##~#
    [ OR  A T
    , AND B T
    , AND C T
    , NOT T T -- !A | !B | !C
    , OR  E J
    , OR  H J
    , AND T J
    , AND D J -- J = (!A | !B | !C) & (E | H) & D
    ]

runRobotEnv :: MonadIO m => RobotEnv m a -> String -> m (a, RobotState)
runRobotEnv (RobotEnv a) input = a `runStateT` RobotState
  { rsInput = input
  , rsValue = Nothing
  }

runRobot
  :: forall mode m. (MonadIO m, Show (ModeName mode), Show (SpringOp mode))
  => Intcode -> SpringScript mode -> m (Maybe Int)
runRobot prog script =
  Intcode.runIntcode prog `runRobotEnv` showSpringScript @mode script >>= \case
    ((Left err, _), _) -> failWith $ show err
    (_,             y) -> pure $ rsValue y

showSpringScript
  :: forall mode. (Show (ModeName mode), Show (SpringOp mode))
  => SpringScript mode -> String
showSpringScript = unlines . (++ [show (MN @mode)]) . map show
