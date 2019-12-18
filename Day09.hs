#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=mtl,megaparsec,text,containers -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Reader
import Control.Monad.Writer

import qualified Input
import Intcode

newtype Env a = Env (ReaderT Int (Writer [Int]) a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput        = Env ask
  envOutput v     = Env $ tell [v]
  envTrace  _ _ _ = Env $ pure Continue

runEnv :: Env a -> Int -> (a, [Int])
runEnv (Env a) = runWriter . runReaderT a

main :: IO ()
main = do
  let
    quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    num16 = [1102,34915192,34915192,7,4,7,99,0]
    big   = [104,1125899906842624,99]
  check "Quine"           quine (== quine)
  check "16-digit number" num16 ((== [16]) . map (length . show))
  check "Big number"      big   (== [big !! 1])

  program <- Input.ints
  putStrLn "Step 1"
  mapM_ print =<< run 1 program
  putStrLn "Step 1"
  mapM_ print =<< run 2 program
  where
    run i p = case runIntcode p `runEnv` i of
      ((Left err, _), _) -> [] <$ putStrLn ("Error: " ++ err)
      ((Right _, _), ys) -> pure ys
    check name p cond = run 1 p >>= \case
      ys | cond ys   -> pure ()
         | otherwise ->
           putStrLn $ "Self-check " ++ name ++ "returned " ++ show ys
