#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script -}

import Data.List (groupBy)

main :: IO ()
main = do
  let range = takeWhile (< 765747) $ dropWhile (< 245318) all6Digits
  putStrLn "Step 1"
  print $ length $ filter hasDup range
  putStrLn "Step 2"
  print $ length $ filter hasPairs range

hasDup, hasPairs :: Int -> Bool
hasDup = any (> 1) . groupLengths
hasPairs = any (== 2) . groupLengths

groupLengths :: Int -> [Int]
groupLengths = map length . groupBy (==) . show

all6Digits :: [Int]
all6Digits = [1..9] >>= up >>= up >>= up >>= up >>= up

up :: Int -> [Int]
up x = (+) (x * 10) <$> [x `mod` 10 .. 9]
