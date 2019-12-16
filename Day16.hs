#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script -}

import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
  input <- takeWhile isDigit <$> getContents
  display $ fft 100 $ parse input
  let input2 = drop (read $ take 7 input) (concat $ replicate 10000 input)
  display $ fft2 100 $ parse input2
  where
    display = putStrLn . concatMap show . take 8

parse :: String -> [Int]
parse = map (fromIntegral . digitToInt)

-- | Generate pattern
-- >>> take 4 (patt 1)
-- [1,0,-1,0]
--
-- >>> take 10 (patt 3)
-- [0,0,1,1,1,0,0,0,-1,-1]
patt :: Int -> [Int]
patt n = tail . concat . repeat $ concatMap (replicate n) [0, 1, 0, -1]

-- | Apply FFT to input
-- >>> fft 1 [1,2,3,4,5,6,7,8]
-- [4,8,2,2,6,1,5,8]
--
-- >>> fft 4 [1,2,3,4,5,6,7,8]
-- [0,1,0,2,9,4,9,8]
--
-- >>> take 8 $ fft 100 $ parse "80871224585914546619083218645595"
-- [2,4,1,7,6,1,7,6]
fft :: Int -> [Int] -> [Int]
fft n = last . take (n + 1) . iterate phase
  where
    phase xs = map step $ zipWith const [1..] xs
      where
        step = lastDigit . sum . zipWith (*) xs . patt

fft2 :: Int -> [Int] -> [Int]
fft2 n = last . take (n + 1) . iterate phase
  where
    phase = (flip seq <*> sum) . (partial <*> (sum . map fromIntegral))
    partial []     _ = []
    partial (x:xs) s = lastDigit s : partial xs (s - fromIntegral x)

lastDigit :: Int -> Int
lastDigit = (`mod` 10) . abs
