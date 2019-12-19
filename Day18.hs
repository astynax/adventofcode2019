#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

import Area
import Errors

newtype Poi = P { getPoi :: Char }
  deriving (Show, Eq, Ord)

data Cell
  = Empty
  | Wall
  | Door Char
  | Poi Poi
  deriving (Show, Eq, Ord)

type MazeMap = AreaMap Cell
type BFSMap = AreaMap (Int, Cell)

type Keys = Set Poi
data Way = Way
  { wayKeys :: !Keys
  , wayStep :: !Int
  , wayPos  :: !Pos
  , wayGoal :: !Poi
  }

type MonadBFS m =
  ( MonadState BFSMap m
  , MonadReader MazeMap m
  , MonadIO m
  )

type RouteMap = HashMap String (Set Poi, Int)

main :: IO ()
main = do
  input <- getContents
  let maze = buildMaze input
  putStrLn "Step 1"
  let ks = [ p | (_, Poi p@(P c)) <- Map.toList maze, c /= '@' ]
  rm <- allWays maze $ P '@' : ks
  print =<< walk rm ks
  putStrLn "Step 2"
  let
    ((cx, cy):_) = [ p | (p, Poi (P '@')) <- Map.toList maze ]
    patch        = Map.fromList
      [ ((cx + x, cy + y), Poi (P c))
      | (y, row) <- zip [-1..1]
        [ "1#2"
        , "###"
        , "3#4"
        ]
      , (x, c) <- zip [-1..1] row
      ]
    maze2        = Map.union patch maze
  rm2 <- allWays maze2 $ map P "1234" ++ ks
  print =<< walk2 rm2 ks

buildMaze :: String -> MazeMap
buildMaze = Area.build fromChar
  where
    fromChar = \case
      '#'           -> Just Wall
      '@'           -> Just $ Poi $ P '@'
      '.'           -> Just Empty
      c | isLower c -> Just $ Poi $ P c
        | isUpper c -> Just (Door $ toLower c)
        | otherwise -> error $ "Wrong char: " ++ show c

walk :: MonadIO m => RouteMap -> [Poi] -> m (Maybe (Int, String))
walk rm keys = do
  cache <- dfs [] 0 (Set.singleton start) start `execStateT` HashMap.empty
  let
    target = tail $ cacheKey start allKeys
    ways   = [v | (k, v) <- HashMap.toList cache, tail k == target]
  pure $ case sortOn fst ways of
    (x:_) -> Just x
    _     -> Nothing
  where
    start = P '@'
    allKeys = Set.fromList $ start : keys
    cacheKey s = map getPoi . (s :) . sort . Set.toList
--    dfs :: Int -> Set Poi -> [Poi] -> Poi -> [Poi] -> _
    dfs path n !ks s =
      gets (HashMap.lookup key) >>= \case
        Just (x, _) | x <= n -> pure ()
        _                    -> do
          modify $ HashMap.insert key (n, reverse path)
          mapM_ try $ Set.toList $ Set.difference allKeys ks
      where
        key = cacheKey s ks
        try e =
          case HashMap.lookup (toKey (s, e)) rm of
            Nothing                  -> error $ "Bad pair:" ++ show (s, e)
            Just (xs, y)
              | Set.isSubsetOf xs ks -> goDeep
              | otherwise            -> pure ()
              where
                goDeep = dfs (getPoi e : path) (n + y) (Set.insert e ks) e

walk2 :: MonadIO m => RouteMap -> [Poi] -> m (Maybe (Int, String))
walk2 rm keys = do
  cache <- dfs [] 0 Set.empty start `execStateT` HashMap.empty
--  mapM_ (liftIO . print) $ sortOn (length . fst) $ HashMap.toList cache
  let
    target = drop 4 $ cacheKey start allKeys
    ways   = [v | (k, v) <- HashMap.toList cache, drop 4 k == target]
  pure $ case sortOn fst ways of
    (x:_) -> Just x
    _     -> Nothing
  where
    start = [P '1', P '2', P '3', P '4']
    allKeys = Set.fromList keys
    cacheKey s = map getPoi . (sort s ++) . sort . Set.toList
    dfs path n !ks starts = do
      liftIO $ print $ length ks
      gets (HashMap.lookup key) >>= \case
        Just (x, _) | x <= n -> pure ()
        _                    -> do
          modify $ HashMap.insert key (n, reverse path)
          forM_ starts $ \s ->
            mapM_ (try s (filter (/= s) starts)) $
              Set.toList $ Set.difference allKeys ks
      where
        key = cacheKey starts ks
        try s ss e =
          case HashMap.lookup (toKey (s, e)) rm of
            Nothing                  -> error $ "Bad pair:" ++ show (s, e)
            Just (xs, y)
              | Set.isSubsetOf xs ks ->
                dfs (getPoi e : path) (n + y) (Set.insert e ks) (e : ss)
              | otherwise            ->
                pure ()

allWays
  :: MonadIO m
  => MazeMap -> [Poi] -> m RouteMap
allWays mz cs = do
  ps <- mapM (uncurry $ bfs mz) pairs
  pure $ HashMap.fromList
    [ (toKey (a, b), v)
    | ((a, b), Just v) <- zip pairs ps ]
  where
    pairs = nub
      [ (x, y)
      | a <- cs
      , b <- cs
      , let [x,y] = sort [a, b]
      , a /= b ]

toKey :: (Poi, Poi) -> String
toKey (P a, P b) = sort [a, b]

-- BFS

tryWay
  :: MonadBFS m
  => Way -> m (Maybe (Keys, Maybe Int))
tryWay (Way ks n pos goal) =
  gets (Map.lookup pos) >>= \case
    Just _ -> pure Nothing
    _      -> do
      m <- ask
      case Map.lookup pos m of
        Nothing           -> failWith $ "Bad pos: " ++ show pos
        Just c
          | c == Poi goal -> pure $ Just (ks, Just n)
          | otherwise     -> do
            modify $ Map.insert pos (n, c)
            case c of
              Wall   -> pure Nothing
              Door k -> pure $ Just (Set.insert (P k) ks, Nothing)
              _      -> pure $ Just (ks,                  Nothing)

bfs
  :: MonadIO m
  => MazeMap -> Poi -> Poi -> m (Maybe (Keys, Int))
bfs mz start end =
  go (fork [way])
    `runReaderT` mz
    `evalStateT` Map.singleton startPos (0, Poi start)
  where
    way = Way
      { wayKeys = Set.empty
      , wayStep = 0
      , wayPos  = startPos
      , wayGoal = end
      }
    startPos = getPos start mz
    go [] = pure Nothing
    go ps = check [] ps >>= \case
      Right p -> pure $ Just p
      Left xs -> go $ fork xs
    check acc []     = pure $ Left acc
    check acc (w:ws) = tryWay w >>= \case
      Just (xs, Just n)  -> pure $ Right (xs, n)
      Just (xs, Nothing) -> check (w { wayKeys = xs } : acc) ws
      Nothing            -> check                       acc  ws

getPos :: Poi -> MazeMap -> Pos
getPos x m = case [ pos | (pos, Poi y) <- Map.toList m, x == y ] of
  [p] -> p
  _   -> error $ "Can't find " ++ show x

fork :: [Way] -> [Way]
fork ways =
  [ Way ks (s + 1) (Area.move d pos) g
  | Way ks s pos g <- ways
  , d <- dirs
  ]
