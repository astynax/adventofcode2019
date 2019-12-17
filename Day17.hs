#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

import Area
import qualified Input
import Intcode

newtype EvalError = EvalError String deriving Show

instance Exception EvalError

newtype Env a = Env { runEnv :: Writer String a }
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput       = Env $ pure 0
  envOutput v    = Env $ tell [chr v]
  envTrace _ _ _ = Env $ pure Continue

data RobotCmd = RR | RL | RF Int deriving (Show)

data Walker = Walker
  { wPath :: ![RobotCmd]
  , wPos  :: !Pos
  , wDir  :: !Dir
  }

type Variant = (Pos, Dir)

type MonadWalker m =
  ( MonadReader (AreaMap Char) m
  , MonadState Walker m
  )

data RobotState = RobotState
  { rsInput  :: [Int]
  , rsOutput :: [Int]
  }

newtype RobotEnv a = RobotEnv (State RobotState a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv RobotEnv where
  envInput       = RobotEnv $ do
    input <- gets rsInput
    case input of
      (x:xs) -> do
        modify $ \s -> s { rsInput = xs }
        pure x
      _      ->
        error "Input buffer underflow!"
  envOutput v    = RobotEnv $
    modify $ \s -> s { rsOutput = rsOutput s ++ [v] }
  envTrace _ _ _ = RobotEnv $ pure Continue

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  pic <- getPicture program
  putStrLn pic
  let area = Area.build fromChar pic
  print $ sum $ map (uncurry (*)) $ crosses area
  putStrLn "Step 2"
  let way = findWay area
  putStrLn $ intercalate "," $ map displayRC way
  let
    input =
      map ord $ unlines
        ["A,B,A,C,A,B,C,B,C,B"
        ,"L,10,R,8,L,6,R,6"
        ,"L,8,L,8,R,8"
        ,"R,8,L,6,L,10,L,10"
        ,"n"
        ] -- TODO: remove hardcode
  vs <- runRobot program input
  putStrLn $ map chr vs
  print $ last vs
  where
    displayRC = \case
      RF x -> show x
      RL   -> "L"
      RR   -> "R"
    fromChar = \case
      '.' -> Nothing
      x   -> Just x
    crosses m =
      [ pos
      | (pos, '#') <- Map.toList m
      , length [ () | (_, _, '#') <- Area.neibs pos m ] == 4
      ]

getPicture :: MonadIO m => Intcode -> m String
getPicture program =
  case runWriter (runEnv $ Intcode.runIntcode program) of
    ((Left err, _), _) -> failWith $ show err
    (_,             y) -> pure y

findWay :: AreaMap Char -> [RobotCmd]
findWay m =
  reverse $ compact $ wPath $
    forward `runReaderT` m `execState` Walker [] sp sd
  where
    (sp, sd) = case [(pos, v) | (pos, v) <- Map.toList m, v `elem` "^v<>"] of
      [(p, v)] -> (p,) $ case v of
        '^' -> U
        'v' -> D
        '<' -> L
        '>' -> R
        _   -> error "Bad robot char"
      _        -> error "Vacuum robot not found!"
    compact []                 = []
    compact [x]                = [x]
    compact (RF x : RF y : xs) = compact $ RF (x + y) : xs
    compact (x:xs)             = x : compact xs

lookAround :: MonadWalker m => m (Maybe Variant, Maybe Variant, Maybe Variant)
lookAround = do
  m <- ask
  Walker _ p d <- get
  pure
    ( variant False m p (turnLeft d)
    , variant True  m p d
    , variant False m p (turnRight d)
    )
  where
    variant mv m p d =
      case Map.lookup np m of
        Just '#' -> Just (if mv then np else p, d)
        _        -> Nothing
      where
        np = move d p

forward :: MonadWalker m => m ()
forward = do
  (l, f, r) <- lookAround
  maybe (pure ()) (>> forward)
    $   (update (RF 1) <$> f)
    <|> (update RL     <$> l)
    <|> (update RR     <$> r)
  where
    update s (p, d) = modify $ \w -> w
      { wPath = s : wPath w
      , wPos  = p
      , wDir  = d
      }

runRobotEnv :: [Int] -> RobotEnv a -> (a, RobotState)
runRobotEnv input (RobotEnv a) = a `runState` RobotState
  { rsInput = input
  , rsOutput = []
  }

runRobot :: MonadIO m => Intcode -> [Int] -> m [Int]
runRobot program input =
  case runRobotEnv input (Intcode.runIntcode p) of
    ((Left err, _), _) -> failWith $ show err
    (_,             y) -> pure $ rsOutput y
  where
    p = 2 : tail program

failWith :: MonadIO m => String -> m a
failWith = liftIO . throwIO . EvalError
