#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script -}

import Data.List
import GHC.Natural

normalDeck, bigDeck, times :: Integer
normalDeck = 10007
bigDeck    = 119315717514047
times      = 101741582076661

main :: IO ()
main = do
  steps <- lines <$> getContents
  putStrLn "Step 1"
  print $ rShuffle normalDeck steps 2019
  putStrLn "Step 2"
  let
    -- https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnifwk
    d = bigDeck
    f = rShuffle d steps
    x = 2020
    y = f x
    z = f y
    a = ((y - z) * invMod (x - y + d) d) `mod` d
    b = (y - a * x) `mod` d
    aNd = powMod a times d
    v = (aNd * x + (aNd - 1) * invMod (a - 1) d * b) `mod` d
  print v

rShuffle :: Integer -> [String] -> Integer -> Integer
rShuffle d = flip (foldl' ap) . reverse
  where
    ap x s = case words s of
      ["deal", "with", "increment", n] -> rInc  d (read n) x
      ["deal", "into", "new", "stack"] -> rDeal d          x
      ["cut", n]                       -> rCut  d (read n) x
      _                                -> error $ "Unknown step: " ++ s

rDeal :: Integer -> Integer -> Integer
rDeal d i = (d - 1 - i) `mod` d

rCut :: Integer -> Integer -> Integer -> Integer
rCut d n i = (i + n + d) `mod` d

rInc :: Integer -> Integer -> Integer -> Integer
rInc d n i = (invMod n d * i) `mod` d

invMod :: Integer -> Integer -> Integer
invMod n d = powMod (n + d) (d - 2) d

powMod :: Integer -> Integer -> Integer -> Integer
powMod n m d = naturalToInteger $ powModNatural (i2n n) (i2n m) (i2n d)
  where
    i2n = naturalFromInteger
