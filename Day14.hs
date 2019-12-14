#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script
  --package=megaparsec,parser-combinators,containers,mtl -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String

import Parsing hiding (count)

newtype Reagent = Reagent String deriving (Show, Eq, Ord, IsString)
data Part = Int :* Reagent deriving (Show, Eq)
data Reaction = (NonEmpty Part) :=> Part deriving (Show, Eq)
-- 10 :* "LEAD" :| [1 :* "PHSTONE"] :=> 1 :* "GOLD"

infixr 6 :*
infixr 4 :=>

type CookBook = Map Reagent Reaction
type Pool = Map Reagent Int
data Lab = Lab
  { labPool   :: !Pool
  , labSource :: !Pool
  } deriving (Show)

type MonadLab m =
  ( MonadReader CookBook m
  , MonadState Lab m
  )

main :: IO ()
main = do
  checkExample exampleCB1 165
  checkExample exampleCB2 13312
  interact $ \s ->
    let
      Right rs = parseLines reactionP s
      cb       = buildCookBook rs
    in unlines
       [ "Step 1"
       , show $ count labSource "ORE" $ produce (1 :* "FUEL") cb cleanLab
       , "Step 2"
       , show $ count labSource "ORE" $ produce (3412429 :* "FUEL") cb cleanLab
       , show $ count labSource "ORE" $ produce (3412430 :* "FUEL") cb cleanLab
       -- just good old manual binary search -----^ :)
       ]

-- parsing

partP :: Parser Part
partP = (:*) <$> decimal <*> (space1 *> reagentP)
  where
    reagentP = Reagent <$> some (satisfy isUpper)

reactionP :: Parser Reaction
reactionP = (:=>)
  <$> partP `sepBy1` (char ',' <* space1)
  <*> (space1 *> string "=>" *> space1 *> partP)

-- reacting

cleanLab :: Lab
cleanLab = Lab Map.empty Map.empty

buildCookBook :: [Reaction] -> CookBook
buildCookBook =
  Map.fromListWithKey nonDup . map ((,) <$> resultName <*> id)
  where
    nonDup (Reagent k) _ _ = error $ "Two reactions for: " ++ k
    resultName (_ :=> _ :* n) = n

produce :: Part -> CookBook -> Lab -> Lab
produce p cb lab =
  makeSome p `runReaderT` cb `execState` lab

makeSome :: MonadLab m => Part -> m ()
makeSome (n :* tr) =
  asks (Map.lookup tr) >>= \case
    Just r@(_ :=> m :* _) -> perform $ (n `from` m) `times` r
    Nothing               ->
      modify $ \lab@Lab{labSource, labPool} -> lab
        { labPool   = record labPool
        , labSource = record labSource
        }
  where
    record = Map.insertWith (+) tr n

use :: MonadLab m => Part -> m ()
use p@(n :* tr) = do
  gets (Map.lookup tr . labPool) >>= \case
    Just  m | m >= n    -> pure ()
            | otherwise -> makeSome $ (n - m) :* tr
    _                   -> makeSome p
  modify $ \lab@Lab{labPool} -> lab
    { labPool = Map.adjust (subtract n) tr labPool
    }

perform :: MonadLab m => Reaction -> m ()
perform (r :| rs :=> n :* tr) = do
  mapM_ use (r:rs)
  modify $ \l@(Lab {labPool}) -> l
    { labPool = Map.insertWith (+) tr n labPool
    }

-- helpers

from :: Int -> Int -> Int
from x y
  | x <= y    = 1
  | otherwise = k + (if r > 0 then 1 else 0)
  where
    (k, r) = divMod x y

count :: (Lab -> Pool) -> Reagent -> Lab -> Int
count l r = Map.findWithDefault 0 r . l

times :: Int -> Reaction -> Reaction
times n (r :| rs :=> tr) = f r :| map f rs :=> f tr
  where
    f (x :* y) = (x * n) :* y

-- example

exampleCB1, exampleCB2 :: CookBook
(exampleCB1:exampleCB2:[]) = map justParse
  [ [ "9 ORE => 2 A"
    , "8 ORE => 3 B"
    , "7 ORE => 5 C"
    , "3 A, 4 B => 1 AB"
    , "5 B, 7 C => 1 BC"
    , "4 C, 1 A => 1 CA"
    , "2 AB, 3 BC, 4 CA => 1 FUEL"
    ]
  , [ "157 ORE => 5 NZVS"
    , "165 ORE => 6 DCFZ"
    , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
    , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
    , "179 ORE => 7 PSHF"
    , "177 ORE => 5 HKGWZ"
    , "7 DCFZ, 7 PSHF => 2 XJWVT"
    , "165 ORE => 2 GPVTF"
    , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
    ]
  ]
  where
    justParse = either (error "Oops!") buildCookBook . mapM (parseIt reactionP)

checkExample :: CookBook -> Int -> IO ()
checkExample cb ore = do
  let lab = produce (1 :* "FUEL") cb cleanLab
  unless (count labSource "ORE" lab == ore) $ do
    putStrLn "---/nSources:"
    mapM_ print $ Map.toList $ labSource lab
    putStrLn "---/nPool:"
    mapM_ print $ Map.toList $ labPool lab
