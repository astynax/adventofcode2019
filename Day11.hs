#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Exception
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import qualified Input
import Intcode

newtype EvalError = EvalError String deriving Show

instance Exception EvalError

data EnvState = EnvState
  { esInput  :: Int
  , esOutput :: Maybe (Int, Maybe Int)  -- 0,1 or 2 ints
  , esHalted :: Bool
  }

newtype Env a = Env (State EnvState a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput     = Env $ gets esInput
  envOutput  v = Env . modify $ \es -> case esOutput es of
    Nothing           -> es { esOutput = Just (v, Nothing) }
    Just (x, Nothing) -> es { esOutput = Just (x, Just v) }
    _                 -> error "To many outputs!"
  envTrace _ _ = Env . \case
    Stp   -> StopAfter <$ modify (\es -> es { esHalted = True })
    Out _ -> gets esOutput >>= \case
      Just (_, Nothing) -> pure StopAfter
      _                 -> pure Continue
    _     -> pure Continue

data Color = Black | White deriving (Show, Eq, Enum)
data Turn = TurnLeft | TurnRight deriving (Show, Eq, Enum)
data Dir = U | D | L | R

data PaintBotState = PaintBotState
  { pbsIntcodeState :: IntcodeState
  , pbsField        :: Map (Int, Int) Color
  , pbsPosition     :: (Int, Int)
  , pbsDirection    :: Dir
  }

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  paint Black program >>= print . length
  putStrLn "Step 2"
  paint White program >>= visualize

paint :: Color -> [Int] -> IO (Map (Int, Int) Color)
paint sc = evalStateT loop . initPaintBot sc
  where
    loop = do
      PaintBotState _ fld pos dir <- get
      let input = fromMaybe Black $ Map.lookup pos fld
      step input >>= \case
        Nothing     -> pure fld
        Just (c, t) -> do
          let
            dir' = rotate t dir
            pos' = move dir' pos
            fld' = Map.insert pos c fld
          modify $ \pbs -> pbs
            { pbsField     = fld'
            , pbsPosition  = pos'
            , pbsDirection = dir'
            }
          loop
    rotate TurnLeft  U = L
    rotate TurnLeft  L = D
    rotate TurnLeft  D = R
    rotate TurnLeft  R = U
    rotate TurnRight U = R
    rotate TurnRight L = U
    rotate TurnRight D = L
    rotate TurnRight R = D
    move U (x, y) = (x, y - 1)
    move D (x, y) = (x, y + 1)
    move L (x, y) = (x - 1, y)
    move R (x, y) = (x + 1, y)

visualize :: Map (Int, Int) Color -> IO ()
visualize m = mapM_ putStrLn rows
  where
    (xs, ys) = unzip . map fst $ Map.toList m
    rows =
      [ [ case fromMaybe Black (Map.lookup (x, y) m) of
            Black -> '.'
            White -> '#'
        | x <- [minimum xs .. maximum xs]
        ]
      | y <- [minimum ys .. maximum ys]
      ]

initPaintBot :: Color -> [Int] -> PaintBotState
initPaintBot c program = PaintBotState
  { pbsIntcodeState = Intcode.initState program
  , pbsField        = Map.singleton (0, 0) c
  , pbsPosition     = (0, 0)
  , pbsDirection    = U
  }

runEnv :: Env a -> Int -> (a, Maybe (Color, Turn))
runEnv (Env a) input = f <$> runState a EnvState
  { esInput  = input
  , esOutput = Nothing
  , esHalted = False
  }
  where
    f = \case
      EnvState _ _                  h | h -> Nothing
      EnvState _ (Just (c, Just t)) _     -> Just (toEnum c, toEnum t)
      _                                   -> error "Too few outputs!"

step
  :: (MonadIO m, MonadState PaintBotState m)
  => Color -> m (Maybe (Color, Turn))
step c = do
  s <- gets pbsIntcodeState
  case runEnv (Intcode.rerunIntcode s) (fromEnum c) of
    ((Left err, _), _) -> failWith $ show err
    ((_, x),        y) -> y <$ modify (\pbs -> pbs { pbsIntcodeState = x })
  where
    failWith = liftIO . throwIO . EvalError
