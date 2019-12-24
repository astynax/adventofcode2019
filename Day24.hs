#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers -}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

import Data.Set (Set)
import qualified Data.Set as Set

type Grid = Set (Int, Int)

data Dir
  = Up
  | Start
  | Down
  deriving (Eq, Show)

data MultiGrid = MG
  { mgDir     :: !Dir
  , mgTops    :: ![Grid]
  , mgGrid    :: !Grid
  , mgBottoms :: ![Grid]
  } deriving (Show)

main :: IO ()
main = do
  grid <- loadGrid <$> getContents
  putStrLn "Step 1"
  let
    gens = iterate evaluate grid
    gen  = findCycle gens
  print $ biodiversity gen
  putStrLn "Step 2"
  let
    mgens = iterate evaluateMG $ single grid
    mg    = mgens !! 200
  print $ countBugs mg

loadGrid :: String -> Grid
loadGrid s = Set.fromList
  [ (x, y)
  | (y, row) <- zip [0..] $ lines s
  , (x, '#') <- zip [0..] row
  ]

-- simple grid

biodiversity :: Grid -> Int
biodiversity = sum . map f . Set.toList
  where
    f (x, y) = 2 ^ (y * 5 + x)

evaluate :: Grid -> Grid
evaluate g = Set.fromList
  [ pos
  | y <- [0..4]
  , x <- [0..4]
  , let pos = (x, y)
  , let n = length $ Set.intersection g (neibSet pos)
  , let member = pos `Set.member` g
  , (member && n == 1) || (not member && (n == 2 || n == 1))
  ]
  where
    neibSet (x, y) = Set.fromList
      [ (x - 1, y)
      , (x + 1, y)
      , (x, y - 1)
      , (x, y + 1)
      ]

findCycle :: [Grid] -> Grid
findCycle = go Set.empty
  where
    go :: Set Int -> [Grid] -> Grid
    go _   []     = error "No way!"
    go acc (x:xs) =
      let b = biodiversity x
      in if b `Set.member` acc
         then x
         else go (Set.insert b acc) xs

-- multi-grid

single :: Grid -> MultiGrid
single g = MG Start [] g []

evaluateMG :: MultiGrid -> MultiGrid
evaluateMG (MG d ts g bs) = MG
  { mgDir     = d
  , mgTops    = if d /= Down then up   else ts
  , mgGrid    = g'
  , mgBottoms = if d /= Up   then down else bs
  }
  where
    g' = eval (headSet ts) g (headSet bs)
    headSet []    = Set.empty
    headSet (s:_) = s
    up = case ts of
      []     ->
        let t = eval Set.empty Set.empty g
        in [t | not (null t)]
      (x:xs) ->
        let (MG _ rs r _) = evaluateMG (MG Up xs x [g])
        in r : rs
    down = case bs of
      []     ->
        let b = eval g Set.empty Set.empty
        in [b | not (null b)]
      (x:xs) ->
        let (MG _ _ r rs) = evaluateMG (MG Down [g] x xs)
        in r : rs

eval :: Grid -> Grid -> Grid -> Grid
eval t g b = Set.fromList
  [ pos
  | y <- [0..4]
  , x <- [0..4]
  , let pos = (x, y)
  , let (nt, ng, nb) = neibs pos
  , let n = sum . map length $
          [ Set.intersection t nt
          , Set.intersection g ng
          , Set.intersection b nb
          ]
  , let member = pos `Set.member` g
  , (x, y) /= (2, 2)
  , (member && n == 1) || (not member && (n == 2 || n == 1))
  ]

neibs :: (Int, Int) -> (Set (Int, Int), Set (Int, Int), Set (Int, Int))
neibs (x, y) = (nt, ng, nb)
  where
    nt = Set.fromList
      [ p | (True, p) <-
        [ (y == 0, (2, 1))
        , (y == 4, (2, 3))
        , (x == 0, (1, 2))
        , (x == 4, (3, 2))
        ]
      ]
    nb = Set.fromList $ concat
      [ ps | (True, ps) <-
        [ (x == 1 && y == 2, line (0,))
        , (x == 3 && y == 2, line (4,))
        , (x == 2 && y == 1, line (,0))
        , (x == 2 && y == 3, line (,4))
        ]
      ]
      where
        line f = map f [0..4]
    ng = Set.fromList
      [ (xx, yy)
      | (xx, yy) <- [u, d, l, r]
      , xx >= 0, xx <= 4
      , yy >= 0, yy <= 4
      ]
    u = (x, y - 1)
    d = (x, y + 1)
    l = (x - 1, y)
    r = (x + 1, y)

countBugs :: MultiGrid -> Int
countBugs (MG _ t g b) = sum (map length t) + length g + sum (map length b)

-- debug

_displayGrid :: Grid -> IO ()
_displayGrid g = mapM_ putStrLn
  [ [ if (x, y) `Set.member` g then '#' else '.'
    | x <- [0..4]
    ]
  | y <- [0..4]
  ]

_displayMultiGrid :: MultiGrid -> IO ()
_displayMultiGrid mg = do
  mapM_ (\g -> putStrLn "-----" >> _displayGrid g) $ reverse $ mgTops mg
  putStrLn "====="
  _displayGrid $ mgGrid mg
  putStrLn "====="
  mapM_ (\g -> _displayGrid g >> putStrLn "-----") $ mgBottoms mg
