#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=vector,mtl,text -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

type MonadComp m = (MonadState Comp m, MonadError String m)

data Comp = Comp
  { pc     :: Int
  , memory :: Vector Int
  } deriving Show

main :: IO ()
main = do
  comp <- initWith . parse <$> TIO.getLine
  putStrLn "Step 1:"
  putStrLn $ evaluate 1202 comp
  putStrLn "Step 2:"
  for_ [0..9999] $ \seed ->
    when (evaluate seed comp == "19690720")
      $ print seed
  where
    evaluate seed s = either id show . runExcept . flip evalStateT s $ do
      let (noun, verb) = seed `divMod` 100
      push 1 noun
      push 2 verb
      eval
      pull 0
    parse :: T.Text -> [Int]
    parse =
      fromMaybe [] . sequence
      . map (readMaybe . T.unpack). T.split (== ',')

eval :: MonadComp m => m ()
eval = do
  (op, a1, a2, a3) <- step
  unless (op == 99) $ do
    void $ case op of
      1 -> apply (+) a1 a2 a3
      2 -> apply (*) a1 a2 a3
      _ -> throwError $ "Unknown OP: " ++ show op
    eval
  where
    apply op a1 a2 a3 = do
      x <- pull a1
      y <- pull a2
      push a3 $ op x y

step :: MonadComp m => m (Int, Int, Int, Int)
step = do
  Comp pos mem <- get
  when (pos > (V.length mem - 4)) $
    throwError "PC is out of bounds!"
  let [op, a1, a2, a3] = V.toList $ V.slice pos 4 mem
  modify (\c -> c{ pc = pos + 4 })
  pure (op, a1, a2, a3)

pull :: MonadComp m => Int -> m Int
pull a = flip V.indexM a =<< gets memory

push :: MonadComp m => Int -> Int -> m ()
push a v = modify $ \c -> c
  { memory = (V.//) (memory c) [(a, v)]
  }

initWith :: [Int] -> Comp
initWith xs = Comp 0 $ V.fromList ys
  where
    -- pad with zeroes
    ys = xs ++ take (4 - length xs `mod` 4) [0, 0, 0]
