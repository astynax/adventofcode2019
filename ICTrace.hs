#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script
  --package=containers,mtl,megaparsec,text
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.State
import Data.IntMap ((!?))
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import qualified Input
import Intcode

data InputStream
  = Cons Int InputStream
  | Forever Int

newtype Env a =
  Env (StateT InputStream IO a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput = Env $ do
    v <- get >>= \case
      Cons x s  -> x <$ put s
      Forever x -> pure x
    liftIO $ putStrLn $ "<<< " ++ show v
    pure v
  envOutput = Env . liftIO . putStrLn . (">>> " ++) . show
  envTrace s (Addr addr) op =
    Env . (Continue <$) . liftIO $ do
      putStr $ show (isRelBase s) ++ "\t/" ++ show addr ++ ":\t"
      putStrLn $ ppOp op
    where
      ppOp = \case
        Stp         -> "STP"
        Jnz p1 p2   -> "JNZ\t" ++ ppP p1 ++ "\t" ++ ppP p2
        Jz  p1 p2   -> "JZE\t" ++ ppP p1 ++ "\t" ++ ppP p2
        Add p1 p2 a -> "ADD\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppPtr a
        Mul p1 p2 a -> "MUL\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppPtr a
        Lt  p1 p2 a -> "LTN\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppPtr a
        Eq  p1 p2 a -> "EQL\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppPtr a
        In        a -> "INP\t" ++ ppPtr a
        Out p1      -> "OUT\t" ++ ppP p1
        Rbs p1      -> "RBS\t" ++ ppP p1
      ppP (Pointer p)   = ppPtr p
      ppP (Immediate x) = show x
      ppPtr a@(APtr (Addr x))  = "@"  ++ show x ++ "(" ++ show (valAt a) ++ ")"
      ppPtr a@(RPtr (RAddr x)) = "@@" ++ show x ++ "(" ++ show (valAt a) ++ ")"
      valAt = fromMaybe 0 . (isMemory s !?) . \case
        APtr (Addr x)  -> x
        RPtr (RAddr x) -> isRelBase s + x

runEnv :: Env a -> InputStream -> IO a
runEnv (Env r) = evalStateT r

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "Usage: ictrace INT [INT...]"
  xs -> do
    let
      intArgs = map read xs
      stream  = foldr Cons (Forever $ last intArgs) (init intArgs)
    program <- Input.ints
    run stream program
  where
    run i p = Intcode.runIntcode p `runEnv` i >>= \case
      (Left err, st) -> putStrLn err >> print st
      _              -> pure ()
