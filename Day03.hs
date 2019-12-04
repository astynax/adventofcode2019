#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=mtl,megaparsec,containers -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

--import Control.Monad.State
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Dir = U | D | L | R deriving (Show)
data Move = Move Dir Int deriving (Show)

main :: IO ()
main = do
  Right moves1 <- readMoves
  Right moves2 <- readMoves
  putStrLn "Step 1:"
  let
    way1 = eval (0, 0) moves1
    way2 = eval (0, 0) moves2
    js   = joints way1 way2
  print $ minimum $ map toDist js
  putStrLn "Step 1:"
  let ws = wires js way1 way2
  print $ minimum ws
  where
    readMoves = runParser movesP "" <$> getLine
    toDist (x, y) = abs x + abs y

    joints xs ys =
      let
        set1 = Set.fromList xs
        set2 = Set.fromList ys
      in Set.toList $ Set.intersection set1 set2

    wires js xs ys = map toWire js
      where
        toWire p = f xs + f ys + 2
          where
            f = length . takeWhile (/= p)

dirP :: (MonadParsec () String m) => m Dir
dirP =
      U <$ char 'U'
  <|> D <$ char 'D'
  <|> L <$ char 'L'
  <|> R <$ char 'R'

moveP :: (MonadParsec () String m) => m Move
moveP = Move <$> dirP <*> decimal

movesP :: (MonadParsec () String m) => m [Move]
movesP = moveP `sepBy` char ','

eval :: (Int, Int) -> [Move] -> [(Int, Int)]
eval _      []              = []
eval p      (Move _ 0 : ms) = eval p ms
eval (x, y) (Move d m : ms) =
  newPos : eval newPos (Move d (m - 1) : ms)
  where
    newPos =
      case d of
        U -> (x, y + 1)
        D -> (x, y - 1)
        L -> (x - 1, y)
        R -> (x + 1, y)
