#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script
  --package=vector,mtl,megaparsec,text,optparse-applicative
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Reader

import qualified Input
import Intcode

newtype Env a =
  Env (ReaderT Int IO a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput = Env $ do
    v <- ask
    liftIO $ putStrLn $ "<<< " ++ show v
    pure v
  envOutput  = Env . liftIO . putStrLn . (">>> " ++) . show
  envTrace _ = Env . liftIO . putStrLn . ppOp
    where
      ppOp = \case
        Stop        -> "STP"
        Jnz p1 p2   -> "JNZ\t" ++ ppP p1 ++ "\t" ++ ppP p2
        Jz  p1 p2   -> "JZE\t" ++ ppP p1 ++ "\t" ++ ppP p2
        Add p1 p2 a -> "ADD\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppA a
        Mul p1 p2 a -> "MUL\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppA a
        Lt  p1 p2 a -> "LTN\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppA a
        Eq  p1 p2 a -> "EQL\t" ++ ppP p1 ++ "\t" ++ ppP p2 ++ "\t" ++ ppA a
        In        a -> "INP\t" ++ ppA a
        Out p       -> "OUT\t" ++ ppP p
      ppP (Position a)  = ppA a
      ppP (Immediate x) =       show x
      ppA (Addr x)      = '@' : show x

runEnv :: Env a -> Int -> IO a
runEnv (Env r) = runReaderT r

main :: IO ()
main = do
  program <- Input.ints
  run 1 program
  where
    run i p = Intcode.runIntcode p `runEnv` i >>= \case
      (Left err, st) -> putStrLn err >> print st
      _              -> pure ()
