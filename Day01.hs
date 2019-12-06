#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script -}

main :: IO ()
main = interact $ solve . parse . lines
  where
    solve input = unlines
      [ "Step 1"
      , show $ solveWith toFuel1 input
      , "Step 2"
      , show $ solveWith toFuel2 input
      ]

parse :: [String] -> [Integer]
parse = map read

solveWith :: (Integer -> Integer) -> [Integer] -> Integer
solveWith = (sum .) . map

toFuel1, toFuel2 :: Integer -> Integer

toFuel1 = subtract 2 . (`div` 3)

toFuel2 x = calc x - x
  where
    calc v
      | next < 0  = v
      | otherwise = v + calc next
      where
        next = div v 3 - 2
