#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Input
import Errors
import Intcode

newtype Pid = Pid { getPid :: Int } deriving (Show, Eq, Ord)

newtype BootEnv m a = BootEnv
  { runBootEnv :: ReaderT Pid m a
  } deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadEnv (BootEnv m) where
  envInput     = BootEnv $ asks getPid
  envOutput  _ = BootEnv $ failWith "Boot: Unexpected output!"
  envTrace _ _ = (.) (BootEnv . pure) $ \case
    In _ -> StopAfter
    _    -> Continue

data ActorState = ActorState
  { actorCounter :: !Int
  , actorInbox   :: ![Int]
  , actorBuffer  :: ![Int]
  , actorWaiting :: !Bool
  }

newtype ActorEnv m a = ActorEnv { runActorEnv :: m a }
  deriving (Functor, Applicative, Monad)

instance
    (MonadIO m, MonadReader Pid m, MonadState Swarm m)
    => MonadEnv (ActorEnv m) where
  envInput = ActorEnv $ zoom $ \s ->
    case actorInbox s of
      []     -> (-1, s{ actorWaiting = True })
      (x:xs) -> (x,  s{ actorWaiting = False, actorInbox = xs })

  envOutput v = ActorEnv $ do
    out <- zoom $ \s ->
      case actorBuffer s ++ [v] of
        (pid:x:y:rest) -> (Just (Pid pid, [x, y]), s{ actorBuffer = rest})
        vs             -> (Nothing,                s{ actorBuffer = vs  })
    case out of
      Just (pid, packet) -> do
        when (pid == Pid 255) $
          liftIO $ putStrLn $ show (getPid pid) ++ " << " ++ show packet
        modify $ flip Map.adjust pid $ \s@ActorState{actorInbox} -> s{
          actorInbox = actorInbox ++ packet
        }
      Nothing       -> pure ()

  envTrace _ _ _ = ActorEnv $ zoom $ \s@ActorState{actorCounter} ->
    ( if actorCounter > 0 then Continue else StopAfter
    , s{ actorCounter = actorCounter - 1 }
    )

type Swarm = Map Pid ActorState

type MonadRuntime m =
  ( MonadIO m
  , MonadState Swarm m
  )

main :: IO ()
main = do
  program <- Input.ints
  print =<< run program

pids :: [Pid]
pids = map Pid [0..49]

boot :: MonadIO m => Intcode -> Pid -> m IntcodeState
boot program pid =
  runBootEnv (Intcode.runIntcode program) `runReaderT` pid >>= \case
    (Left err, _) -> failWith $ show err
    (_,        x) -> pure x

work :: MonadRuntime m => Pid -> IntcodeState -> m IntcodeState
work pid s = do
  modify $ Map.adjust (\as -> as{ actorCounter = 50 }) pid
  runActorEnv (Intcode.rerunIntcode s) `runReaderT` pid >>= \case
    (Left err, _)  -> failWith $ show err
    (_,        is) -> pure is

run :: MonadIO m => Intcode -> m Int
run program = do
  actors <- zip pids <$> mapM (boot program) pids
  loop [] actors `evalStateT` Map.fromList
    [ (pid, ActorState 0 [] [] False)
    | pid <- nat : pids
    ]
  where
    loop acc []             = do
      result <- idle >>= \case
        False -> pure Nothing
        _     -> trottle
      case result of
        Just x  -> pure x
        Nothing -> loop [] $ reverse acc
    loop acc ((pid, is):as) = do
      is' <- work pid is
      loop ((pid, is'):acc) as

    trottle =
      gets (Map.lookup nat) >>= \case
        Nothing                                  -> error "Impossible!"
        Just ActorState{actorInbox,actorCounter} ->
          case reverse actorInbox of
            (y:x:_)
              | y == actorCounter -> pure $ Just y
              | otherwise         -> do
                liftIO $ putStrLn $ "TROTTLE: " ++ show [x, y]
                modify $
                  Map.adjust (\s -> s{actorInbox = [], actorCounter = y}) nat .
                    Map.adjust (\s -> s{actorInbox = [x, y] }) (Pid 0)
                pure Nothing
            _       -> pure Nothing
    idle = do
      m <- get
      pure $ null
        [ () | (pid, ActorState{actorWaiting=w}) <- Map.toList m
        , not w, pid /= nat ]
    nat = Pid 255

-- mailboxing

zoom
  :: (MonadIO m, MonadReader Pid m, MonadState Swarm m)
  => (ActorState -> (a, ActorState)) -> m a
zoom f = do
  pid <- ask
  gets (Map.lookup pid) >>= \case
    Nothing -> failWith $ "Bad pid: " ++ show (getPid pid)
    Just as ->
      let (a, as') = f as
      in a <$ modify (Map.insert pid as')
