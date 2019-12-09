#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import qualified Input
import qualified Intcode

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  run 1 program
  putStrLn "Step 2"
  run 5 program
  where
    run i p = Intcode.runSimpleEnv (Intcode.runIntcode p) i >>= \case
      (Left err, st) -> putStrLn err >> print st
      _              -> pure ()
