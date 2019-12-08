#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script -}

import Data.List (unfoldr, sortOn)

newtype Layer = Layer { getLayer :: [[Char]] }

instance Semigroup Layer where
  Layer l1 <> Layer l2 = Layer $ zipWith (zipWith f) l1 l2
    where
      f '2' x = x
      f x   _ = x

instance Monoid Layer where
  mempty = Layer $ replicate 6 $ replicate 25 '2'

main :: IO ()
main = do
  image <- map Layer . chunk 6 . chunk 25 <$> getLine
  let
    layer = head $ sortOn (count '0') image
    ones = count '1' layer
    twos = count '2' layer
  putStrLn "Step 1:"
  print $ ones * twos
  putStrLn "Step 1:"
  mapM_ (putStrLn . map toPixel) $ getLayer $ mconcat image
  where
    toPixel '0' = '.'
    toPixel '1' = '*'
    toPixel _   = ' '

chunk :: Int -> [a] -> [[a]]
chunk = unfoldr . maybeSplitAt
  where
    maybeSplitAt _ [] = Nothing
    maybeSplitAt n xs = Just $ splitAt n xs

count :: Char -> Layer -> Int
count x = length . filter (== x) . concat . getLayer
