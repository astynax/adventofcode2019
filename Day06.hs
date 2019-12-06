#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers -}

{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Graph = Map String [String]
type Input = [(String, String)]

test1, test2 :: String

test1 = unlines
  [ "COM)B"
  , "B)C"
  , "C)D"
  , "D)E"
  , "E)F"
  , "B)G"
  , "G)H"
  , "D)I"
  , "E)J"
  , "J)K"
  , "K)L"
  ]

test2 = test1 ++ unlines
  [ "K)YOU"
  , "I)SAN" ]

main :: IO ()
main = do
  unless (solve1 test1 == "42") $ putStrLn "Self-test #1 failed!"
  unless (solve2 test2 == "4") $ putStrLn "Self-test #2 failed!"
  interact solve
  where
    solve input = unlines
      [ "Step 1"
      , solve1 input
      , "Step 2"
      , solve2 input
      ]
    solve1 = show . sum . map snd . Map.toList . annotate . buildMap . parse
    solve2 = show . subtract 2 . length . walk "YOU" "SAN" . buildMap2 . parse
    -- here I subtract 2 because of ["YOU", first orbit] in path

parse :: String -> Input
parse = map (fmap tail . break (== ')')) . lines

buildMap :: Input -> Graph
buildMap = foldl' push Map.empty
  where
    push m (k, v) = Map.insertWith (++) k [v] m

buildMap2 :: Input -> Graph
buildMap2 = foldl' push Map.empty
  where
    push m (k, v) =
      Map.insertWith   (++) k [v] $
        Map.insertWith (++) v [k] m

annotate :: Graph -> Map String Int
annotate m = go Map.empty (0, "COM")
  where
    go acc (n, k) =
      let acc' = Map.insert k n acc
      in case Map.lookup k m of
        Nothing -> acc'
        Just xs -> foldl' go acc' $ map (n + 1,)xs

walk :: String -> String -> Graph -> [String]
walk f t m = go [] f
  where
    go path x
      | x `elem` path = []
      | otherwise     =
        case Map.lookup x m of
          Nothing -> error "Oops!"
          Just xs
            | t `elem` xs -> reverse path'
            | otherwise   ->
              case filter (not . null) (map (go path') xs) of
                []    -> []
                (p:_) -> p
      where
        path' = x : path
