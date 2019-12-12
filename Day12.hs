#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=megaparsec,mtl,containers -}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Bifunctor as Bifunctor
import Data.List (sortOn)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Void
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Tri t a = Tri
  { tx :: !a
  , ty :: !a
  , tz :: !a
  } deriving (Show, Functor, Foldable, Eq)

data Pos
data Vel

type Moon = (Tri Pos Int, Tri Vel Int)

main :: IO ()
main = interact $ \input ->
  let
    Right ps = sequence $ map parsePos $ lines input
    moons = map (, initVel) ps
  in unlines
     [ "Step 1"
     , show $ solution1For moons
     , "Step 2"
     , show $ solution2For moons
     ]
  where
    solution1For, solution2For :: [Moon] -> Int
    solution1For = sum . map totalEnergy . snd . runSim ((== 1000) . fst)
    solution2For ms =
      let
        periodX = runAxis tx
        periodY = runAxis ty
        periodZ = runAxis tz
      in lcm periodX $ lcm periodY periodZ
      where
        runAxis :: (forall t. Tri t Int -> Int) -> Int
        runAxis f = fst $ runSim (\(n, m) -> n > 0 && axis f m == axis f ms) ms
        axis :: (forall t. Tri t a -> a) -> [(Tri t1 a, Tri t2 a)] -> [(a, a)]
        axis f = map (Bifunctor.bimap f f)

runSim :: ((Int, [Moon]) -> Bool) -> [Moon] -> (Int, [Moon])
runSim cond ms = Bifunctor.second unMap
  $ runReaderT (simulate cond 0) ps `runState` m
  where
    unMap = map snd . Map.toList
    m = Map.fromList $ zip [0..] ms
    ps = pairs $ zipWith const [0 :: Int ..] ms

simulate
  :: (Ord k, MonadReader [(k, k)] m, MonadState (Map k Moon) m)
  => ((Int, [Moon]) -> Bool)
  -> Int
  -> m Int
simulate cond n = do
  m <- map snd . sortOn fst . Map.toList <$> get
  if cond (n, m)
    then pure n
    else do
      ask >>= mapM_ (modify . updateVels)
      modify $ Map.map applyVel
      simulate cond (n + 1)
  where
    updateVels (k1, k2) m = case (m !? k1, m !? k2) of
      (Just (pos1, vel1), Just (pos2, vel2)) ->
        let deltas = liftTri2 grav pos1 pos2
        in  Map.insert k1 (pos1, liftTri2 (+) vel1 (fst <$> deltas))
          $ Map.insert k2 (pos2, liftTri2 (+) vel2 (snd <$> deltas)) $ m
      _                                      ->
        error "Impossible!"
    applyVel (pos, vel) = (liftTri2 (+) pos vel, vel)
    grav p1 p2
      | p1 < p2   = (1, -1)
      | p1 > p2   = (-1, 1)
      | otherwise = (0,  0)

initVel :: Tri Vel Int
initVel = Tri 0 0 0

parsePos :: String -> Either String (Tri Pos Int)
parsePos = Bifunctor.first errorBundlePretty . runParser triP ""

triP :: Parsec Void String (Tri Pos Int)
triP = Tri
  <$> (string  "<x=" *> intP)
  <*> (string ", y=" *> intP)
  <*> (string ", z=" *> intP <* string ">")
  where
    intP = signed (pure ()) decimal

totalEnergy :: Moon -> Int
totalEnergy (pos, vel) = en pos * en vel
  where
    en :: Tri t Int -> Int
    en = sum . fmap abs

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

liftTri2 :: (a -> b -> c) -> Tri t1 a -> Tri t2 b -> Tri t3 c
liftTri2 f (Tri a b c) (Tri x y z) = Tri (f a x) (f b y) (f c z)
