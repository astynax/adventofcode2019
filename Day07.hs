#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=vector,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Exception
import Control.Monad.State
import Data.List (nub)

import qualified Input
import Intcode

newtype EvalError = EvalError String deriving Show

instance Exception EvalError

data EnvState = EnvState
  { esInput   :: [Int]
  , esOutput  :: Maybe Int
  , esHalted  :: Bool
  , esLastPos :: Addr
  }

newtype Env a = Env (State EnvState a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput = Env $ gets esInput >>= \case
    (x:xs) -> x <$ modify (\es -> es { esInput = xs})
    _      -> error "Input underflow!"
  envOutput v = Env $ modify $ \es -> es { esOutput = Just v }
  envTrace _ a op = Env $ do
    modify $ \es -> es { esLastPos = a }
    gets esOutput >>= \case
      Nothing ->
        True <$ modify (\es -> es { esHalted = esHalted es || op == Stop })
      Just _  ->
        pure False

runEnv :: Env a -> Maybe Int -> Int -> (a, EnvState)
runEnv (Env a) phase input = runState a EnvState
  { esInput   = [x | Just x <- [phase, Just input]]
  , esOutput  = Nothing
  , esHalted  = False
  , esLastPos = Addr 0
  }

main :: IO ()
main = do
  run1
    [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33
    ,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] (1,0,4,3,2) >>= \case
      65210 -> pure ()
      _     -> putStrLn "Self-check #1 failed!"
  run2
    [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54
    ,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4
    ,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] (9,7,8,5,6) >>= \case
      18216 -> pure ()
      _     -> putStrLn "Self-check #2 failed!"
  program <- Input.ints
  print =<< maximum <$> mapM (run1 program) (phases 0)
  print =<< maximum <$> mapM (run2 program) (phases 5)
  where
    justs (a, b, c, d, e) = (Just a, Just b, Just c, Just d, Just e)
    initStates p = (s, s, s, s, s) where s = Intcode.initState p
    run i (sa, sb, sc, sd, se) (a, b, c, d, e) =
      flip runStateT i $ (,,,,)
        <$> amp sa a
        <*> amp sb b
        <*> amp sc c
        <*> amp sd d
        <*> amp se e
      where
        amp s phase = do
          input <- get
          case runEnv (Intcode.rerunIntcode s) phase input of
            ((Left err, _), _) -> failWith $ show err
            ((_, x),       es) -> case es of
              EnvState _ (Just o) _ p   ->
                (False, x { isPC = p }) <$ put o
              EnvState _ Nothing True _ ->
                pure (True, x)
              _                            ->
                failWith "No output and halt!"
        failWith = liftIO . throwIO . EvalError
    run1 p = fmap snd . run 0 (initStates p) . justs
    run2 p = go 0 (initStates p) . justs
      where
        go i ss ps = run i ss ps >>= \case
          (((True, _), (True, _), (True, _), (True, _), (True, _)), v) -> do
            pure v
          (((False, sa), (False, sb), (False, sc), (False, sd), (False, se)), v) -> do
            go v (sa, sb, sc, sd, se) (Nothing, Nothing, Nothing, Nothing, Nothing)
          _ -> throwIO $ EvalError "Non-consistent amps!"

phases :: Int -> [(Int, Int, Int, Int, Int)]
phases s =
  [ (s + a, s + b, s + c, s + d, s + e)
  | a <- [0..4]
  , b <- [0..4]
  , c <- [0..4]
  , d <- [0..4]
  , e <- [0..4]
  , length (nub [a,b,c,d,e]) == 5
  ]
