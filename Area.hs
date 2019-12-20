{-# LANGUAGE TupleSections #-}

module Area
  ( AreaMap
  , Pos
  , Dir(..)
  , dirs
  , visualize, build
  , move, turnLeft, turnRight
  , neibs
  , bounds
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
    ((nx, ny), (mx, my)) = bounds m
    rows =
      [ [ toChar $ Map.lookup (x, y) m
        | x <- [nx .. mx]
        ]
      | y <- [ny .. my]
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

bounds :: AreaMap a -> ((Int, Int), (Int, Int))
bounds m = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (xs, ys) = unzip $ Map.keys m
