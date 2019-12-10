#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=mtl,containers -}

{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List
import Data.Set (Set, member)
import qualified Data.Set as Set

type StarMap = (Int, Int, Set (Int, Int))

main :: IO ()
main = do
  checkNeibs
  showExample
  interact $ \input ->
    let
      sm@(_, _, s) = readMap input
      (pos, n) =
        head $ reverse $ sortOn snd
          [ (p, v)
          | p <- Set.toList s
          , let (_, _, v) = countVisible sm p
          ]
    in unlines
       [ "Step 1"
       , show n
       , "Step 2"
       , unlines
         [ show idx ++ ") " ++ show x ++ "," ++ show y
         | (idx, (x, y)) <- zip [1 :: Int ..] $ shootOut sm pos ]
       ]

showExample :: IO ()
showExample = do
  let testMap = [ ".#..#"
                , "....."
                , "#####"
                , "....#"
                , "...##" ]
  let sm = readMap $ unlines testMap
  putStrLn $ unlines
    [ [ if c == '#' then head (show n) else '.'
      | (x, c) <- zip [0..] row
      , let (_, _, n) = countVisible sm (x, y) ]
    | (y, row) <- zip [0..] testMap
    ]

readMap :: String -> StarMap
readMap s = (width, height, set)
  where
    cells = zip [0..] . map (zip [0..]) . lines $ s
    set = foldr Set.union Set.empty $ map toSet cells
    height = length cells
    width = case cells of
      []          -> 0
      ((_, x): _) -> length x
    toSet (y, xs) = Set.fromList
      [ (x, y)
      | (x, c) <- xs
      , c == '#' ]

countVisible :: StarMap -> (Int, Int) -> (Set (Int, Int), [(Int, Int)], Int)
countVisible (w, h, m) base@(x, y) = foldl' f (Set.empty, [], 0) ns
  where
    f old@(counted, saved, n) p
      | p `member` m && not (p `member` counted) =
        ( Set.union (Set.intersection m $ castRay size base p) counted
        , p : saved
        , n + 1 )
      | otherwise = old
    size = max w h
    ns =
      [ (nx, ny)
      | (dx, dy) <- neibs size
      , let nx = dx + x
      , let ny = dy + y
      , nx >= 0 && nx < w
      , ny >= 0 && ny < h ]

castRay :: Int -> (Int, Int) -> (Int, Int) -> Set (Int, Int)
castRay s (x, y) (ax, ay) = Set.fromList
  [ (ax + dx, ay + dy)
  | n <- [0 .. (1 + s `div` (max 1 $ min (abs sx) (abs sy)))]
  , let dx = sx * n
  , let dy = sy * n
  , abs dx <= s
  , abs dy <= s ]
  where
    sx' = ax - x
    sy' = ay - y
    g = gcd sx' sy'
    sx = sx' `div` g
    sy = sy' `div` g

neibs :: Int -> [(Int, Int)]
neibs r = go 1
  where
    go n
      | n > r = []
      | otherwise = concat
        [ init $ map (,-n) ds  -- a) aad
        , tail $ map (-n,) ds  -- b) b d
        , tail $ map (,n) ds   -- c) bcc
        , init $ map (n,) ds   -- d)
        , go (n + 1) ]
        where
          ds = [-n .. n]

checkNeibs :: IO ()
checkNeibs = unless t $ putStrLn "neibs check failed!"
  where
    t = l == nub l && sort l == sort p
    l = neibs 5
    p =
      [ (dx, dy)
      | dx <- [-5 .. 5]
      , dy <- [-5 .. 5]
      , dx /= 0 || dy /= 0 ]

angleFor :: (Int, Int) -> (Int, Int) -> Float
angleFor (x, y) (x2, y2)
  | dx == 0 && dy <  0  = 0
  | dx == 0 && dy >  0  = 180
  | dx >  0 && dy == 0  = 90
  | dx <  0 && dy == 0  = 270
  | dx >  0 && dy <  0  = 90  + angle
  | dx >  0 && dy >  0  = 180 - angle
  | dx <  0 && dy >  0  = 180 - angle
  | otherwise           = 360 - angle
  where
    angle = 180 * atan (dx / dy) / pi
    dx = diff x2 x
    dy = diff y2 y
    diff a b = fromIntegral a - fromIntegral b

shootOut :: StarMap -> (Int, Int) -> [(Int, Int)]
shootOut sm base = go sm
  where
    myself = Set.singleton base
    go old@(w, h, m)
      | m == myself = []
      | otherwise   = xs ++ go (w, h, Set.difference m $ Set.fromList xs)
      where
        (_, ps, _) = countVisible old base
        xs = map fst $ sortOn snd $ map ((,) <$> id <*> angleFor base) ps
