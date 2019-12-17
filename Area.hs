{-# LANGUAGE TupleSections #-}

module Area
  ( AreaMap
  , Pos
  , Dir(..)
  , dirs
  , visualize, build
  , move, turnLeft, turnRight
  , neibs
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

type Pos = (Int, Int)
type AreaMap v = Map Pos v

data Dir = U | D | L | R deriving (Show, Eq, Enum)

dirs :: [Dir]
dirs = [U, D, L, R]

visualize :: (Maybe v -> Char) -> AreaMap v -> IO ()
visualize toChar m = mapM_ putStrLn rows
  where
    (xs, ys) = unzip . map fst $ Map.toList m
    rows =
      [ [ toChar $ Map.lookup (x, y) m
        | x <- [minimum xs .. maximum xs]
        ]
      | y <- [minimum ys .. maximum ys]
      ]

move :: Dir -> Pos -> Pos
move d (x, y) = case d of
  U -> (x    , y - 1)
  D -> (x    , y + 1)
  L -> (x - 1, y    )
  R -> (x + 1, y    )

turnLeft :: Dir -> Dir
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

neibs :: Pos -> AreaMap v -> [(Dir, Pos, v)]
neibs pos m = mapMaybe f dirs
  where
    f d = (d, p,) <$> Map.lookup p m
      where
        p = move d pos

build :: (Char -> Maybe a) -> String -> AreaMap a
build toCell s = Map.fromList
  [ ((x, y), v)
  | (y, row) <- zip [0..] $ lines s
  , (x, Just v) <- map (fmap toCell) $ zip [0..] row
  ]
