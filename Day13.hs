#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.IO (stdin, hSetBuffering, BufferMode(..))

import qualified Input
import Intcode

newtype EvalError = EvalError String deriving Show

instance Exception EvalError

type GameMap = Map (Int, Int) Int

data GameState = GameState
  { gsIntcodeState :: IntcodeState
  , gsField        :: GameMap
  }

data EnvState = EnvState
  { esInput  :: Int
  , esOutput :: [Int]
  , esHalted :: Bool
  }

newtype Env a = Env (State EnvState a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput     = Env $ gets esInput
  envOutput  v = Env . modify $ \es -> es { esOutput = esOutput es ++ [v] }
  envTrace _ _ = Env . \case
    Stp   -> StopAfter <$ modify (\es -> es { esHalted = True })
    Out _ -> gets esOutput >>= \case
      xs | length xs == 2 -> pure StopAfter
      _                   -> pure Continue
    _     -> pure Continue

data LiveEnvState = LiveEnvState
  { lesOutput :: [Int]
  , lesField  :: GameMap
  }

newtype LiveEnv a = LiveEnv
  { runLiveEnv :: ReaderT (IORef LiveEnvState) IO a
  } deriving (Functor, Applicative, Monad)

instance MonadEnv LiveEnv where
  envInput     = LiveEnv $ ask >>= \ref -> liftIO $ do
    s <- readIORef ref
    let
      field = lesField s
      cells = Map.toList field
      (((px, _), _):_) = filter ((== 3) . snd) cells
      (((bx, _), _):_) = filter ((== 4) . snd) cells
    visualize field
    threadDelay 50000
    pure $ if px < bx then 1 else if px > bx then -1 else 0  -- AI! :)
  --   getChar >>= \case
  --       'j' -> pure (-1)
  --       'l' -> pure 1
  --       _   -> pure 0
  envOutput  v = LiveEnv $ ask >>= \ref -> liftIO $ do
    s <- readIORef ref
    writeIORef ref $ case lesOutput s of
      [x, y] -> s
        { lesOutput = []
        , lesField  = Map.insert (x, y) v $ lesField s
        }
      vs     -> s
        { lesOutput = vs ++ [v]
        }
  envTrace _ _ _ = pure Continue

main :: IO ()
main = do
  program <- Input.intsFile "Day13.input"
  putStrLn "Step 1"
  play1 program >>= print
  putStrLn "Step 2"
  hSetBuffering stdin NoBuffering
  play2 program >>= print
  hSetBuffering stdin LineBuffering

play1 :: [Int] -> IO Int
play1 = evalStateT loop . initGame
  where
    loop = step 0 >>= \case
      Just [x, y, t] -> do
        modify $ \gs -> gs { gsField = Map.insert (x, y) t $ gsField gs }
        loop
      Nothing        ->
        length . filter ((== 2) . snd) . Map.toList <$> gets gsField
      Just os        ->
        failWith $ "Bad output: " ++ show os

play2 :: [Int] -> IO Int
play2 program = do
  ref <- newIORef $ LiveEnvState
    { lesOutput = []
    , lesField  = Map.empty
    }
  runReaderT (runLiveEnv $ Intcode.runIntcode $ 2 : tail program) ref >>= \case
    (Left err, _) -> failWith $ show err
    _             -> do
      s <- readIORef ref
      pure $ fromMaybe 0 $ Map.lookup (-1, 0) $ lesField s

visualize :: Map (Int, Int) Int -> IO ()
visualize m
  | length m == 0 = pure ()
  | otherwise     = mapM_ putStrLn rows
  where
    (xs, ys) = unzip . map fst $ Map.toList m
    rows =
      [ [ case fromMaybe 0 (Map.lookup (x, y) m) of
            1 -> '#'
            2 -> '$'
            3 -> '~'
            4 -> '.'
            _ -> ' '
        | x <- [minimum xs .. maximum xs]
        ]
      | y <- [minimum ys .. maximum ys]
      ]

initGame :: [Int] -> GameState
initGame program = GameState
  { gsIntcodeState = Intcode.initState program
  , gsField        = Map.empty
  }

runEnv :: Env a -> Int -> (a, Maybe [Int])
runEnv (Env a) input = unpack <$> runState a EnvState
  { esInput  = input
  , esOutput = []
  , esHalted = False
  }
  where
    unpack (EnvState _ os f)
      | f         = Nothing
      | otherwise = Just os

step
  :: (MonadIO m, MonadState GameState m)
  => Int -> m (Maybe [Int])
step i = do
  s <- gets gsIntcodeState
  case runEnv (Intcode.rerunIntcode s) i of
    ((Left err, _), _) -> failWith $ show err
    ((_, x),        y) -> y <$ modify (\gs -> gs { gsIntcodeState = x })

failWith :: MonadIO m => String -> m a
failWith = liftIO . throwIO . EvalError
