#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=mtl,containers -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import Area

data Level
  = Zero
  | Next Level
  deriving (Show, Eq, Ord)

data PortalKind
  = In
  | Out
  deriving (Show, Eq)

data Cell
  = Exit
  | Floor
  | Portal PortalKind Pos
  deriving (Show)

data Portal = P
  { pName :: !String
  , pKind :: !PortalKind
  , pPos  :: !Pos
  , pDest :: !Pos
  , pTail :: !Pos
  } deriving (Show, Eq)

data Maze = Maze
  { mazeMap      :: !MazeMap
  , mazeEntrance :: !Pos
  } deriving (Show)

type MazeMap = AreaMap Cell
type BFSMap = Map (Pos, Level) Int

data Way = Way
  { _wayStep :: !Int
  , wayPos   :: !Pos
  , wayLevel :: !Level
  }

type MonadBFS m =
  ( MonadState BFSMap m
  , MonadReader MazeMap m
  , MonadIO m
  )

main :: IO ()
main = do
  input <- getContents
  let Right maze = buildMaze input
  -- _debugMaze maze
  print =<< bfs maze

buildMaze :: String -> Either String Maze
buildMaze input = do
  entrance <- findSingle "AA"
  exit <- findSingle "ZZ"
  portals <- do
    eps <- fmap (mapM snd . Map.toList) $
      (\f -> foldM f Map.empty allPortals) $
        \m portal@P{pName, pDest = d1} -> if
          | portal == entrance -> pure m
          | portal == exit     -> pure m
          | otherwise       ->
            let store x = Right $ Map.insert pName x m
            in case Map.lookup pName m of
              Nothing                    -> store $ Left portal
              Just (Left p2@P{pDest = d2}) ->
                store $ Right [portal{pDest = d2}, p2{pDest = d1}]
              _                           ->
                Left $ "Third portal: " ++ pName ++ " at " ++ show (pPos portal)
    case eps of
      Left P{pPos} -> Left $ "Unmatched portal at " ++ show pPos
      Right pps    -> pure $ concat pps
  cells <- do
    let
      floors =
        Map.insert (pDest exit) Exit $
        Map.delete (pPos  exit) $
        Map.delete (pTail exit) $
        Map.delete (pPos  entrance) $
        Map.delete (pTail entrance) $
          flip Map.map cm $ \case
            '.' -> Floor
            _   -> Portal In (-1, -1)
      cs = (\f -> foldl' f floors portals) $
        \m P{pKind, pPos, pDest, pTail} ->
          Map.insert pPos (Portal pKind pDest) $
            Map.delete pTail m
    case [p | (p, Portal _ (-1, -1)) <- Map.toList cs] of
      [] -> pure cs
      ps  -> Left $ "Bad portals: " ++ show ps
  pure $ Maze cells $ pDest entrance
  where
    cm = buildCharMap input
    allPortals = collectPortals cm
    findSingle x =
      case [p | p@P{pName} <- allPortals, pName == x] of
        [y] -> Right y
        _   -> Left $ "Can't find a single " ++ x

buildCharMap :: String -> AreaMap Char
buildCharMap = Area.build $ \case
  c | isUpper c -> Just c
  '.'           -> Just '.'
  _             -> Nothing

collectPortals :: AreaMap Char -> [Portal]
collectPortals m = mapMaybe toPortal
  [ (pos, c) | (pos, c) <- Map.toList m, isUpper c ]
  where
    toPortal (p, c) =
      case neibs p m of
        [(U, t, z),   (D, d, '.')] | isUpper z -> mkP z c d t
        [(U, d, '.'), (D, t, z)  ] | isUpper z -> mkP c z d t
        [(L, t, z),   (R, d, '.')] | isUpper z -> mkP z c d t
        [(L, d, '.'), (R, t, z)  ] | isUpper z -> mkP c z d t
        _                                      -> Nothing
      where
        mkP a b d t@(x, y) = Just $ P [a, b] k p d t
          where
            k | x == nx || x == mx || y == ny || y == my = Out
              | otherwise                                = In
    ((nx, ny), (mx, my)) = bounds m

-- BFS

bfs
  :: MonadIO m
  => Maze -> m (Maybe Int)
bfs (Maze maze start) =
  go (fork [Way 0 start Zero])
    `runReaderT` maze
    `evalStateT` Map.singleton (start, Zero) 0
  where
    go [] = pure Nothing
    go ps = check [] ps >>= \case
      Right p -> pure $ Just p
      Left xs -> go $ fork xs
    check acc []     = pure $ Left acc
    check acc (w:ws) = tryWay w >>= \case
      Just (Right n)     -> pure $ Right n
      Just (Left (p, l)) -> check (w {wayPos = p, wayLevel = l} : acc) ws
      Nothing            -> check                                 acc  ws


tryWay
  :: MonadBFS m
  => Way -> m (Maybe (Either (Pos, Level) Int))
tryWay (Way n pos lvl) =
  gets (Map.lookup (pos, lvl)) >>= \case
    Just _ -> pure Nothing
    _      -> do
      m <- ask
      case (Map.lookup pos m, lvl) of
        (Nothing,             _)      -> pure Nothing
        (Just Exit,           Zero)   -> pure $ Just $ Right n
        (Just Exit,           _)      -> pure Nothing
        (Just (Portal In p),  _)      -> pure $ Just $ Left (p, Next lvl)
        (Just (Portal Out _), Zero)   -> pure Nothing
        (Just (Portal Out p), Next l) -> pure $ Just $ Left (p, l)
        (Just Floor,          _)      -> do
          modify $ Map.insert (pos, lvl) n
          pure $ Just $ Left (pos, lvl)

fork :: [Way] -> [Way]
fork ways =
  [ Way (s + 1) (Area.move d pos) lvl
  | Way s pos lvl <- ways
  , d <- dirs
  ]

-- examples and debugging

_minimalExample :: String
_minimalExample = unlines
  [ "   E A   "
  , "   D A   "
  , "  #.#.#  "
  , "BC.....BC"
  , "  #.#.#  "
  , "   E Z   "
  , "   D Z   "
  ]

_example :: String
_example = unlines
  [ "         A           "
  , "         A           "
  , "  #######.#########  "
  , "  #######.........#  "
  , "  #######.#######.#  "
  , "  #######.#######.#  "
  , "  #######.#######.#  "
  , "  #####  B    ###.#  "
  , "BC...##  C    ###.#  "
  , "  ##.##       ###.#  "
  , "  ##...DE  F  ###.#  "
  , "  #####    G  ###.#  "
  , "  #########.#####.#  "
  , "DE..#######...###.#  "
  , "  #.#########.###.#  "
  , "FG..#########.....#  "
  , "  ###########.#####  "
  , "             Z       "
  , "             Z       "
  ]

_debugMaze :: Maze -> IO ()
_debugMaze (Maze m s) = do
  Area.visualize (fromMaybe ' ') $
    Map.union (Map.insert s '@' pm) $ Map.mapWithKey f m
  mapM_ print [(pos, p) | (pos, p@(Portal _ _)) <- Map.toList m]
  where
    pm = snd $ foldl' ins ('a', Map.empty) [p | (_, Portal _ p) <- Map.toList m]
    ins (c, acc) p = (succ c, Map.insert p c acc)
    f pos = \case
      Exit       -> '!'
      Floor      -> fromMaybe '.'     $ Map.lookup pos pm
      Portal _ p -> maybe '?' toUpper $ Map.lookup p   pm
