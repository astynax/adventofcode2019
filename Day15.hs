#!/usr/bin/env stack
{- stack --resolver=lts-13.24 script --package=containers,mtl,megaparsec,text -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import Area
import qualified Input
import Intcode

newtype EvalError = EvalError String deriving Show

instance Exception EvalError

data EnvState = EnvState
  { esInput  :: Int
  , esOutput :: Maybe Int
  }

newtype Env a = Env (State EnvState a)
  deriving (Functor, Applicative, Monad)

instance MonadEnv Env where
  envInput     = Env $ gets esInput
  envOutput  v = Env . modify $ \es -> case esOutput es of
    Nothing           -> es { esOutput = Just v }
    _                 -> error "To many outputs!"
  envTrace _ _ = Env . \case
    Stp   -> pure StopAfter
    Out _ -> pure StopAfter
    _     -> pure Continue

data Cell = Wall | Empty | Oxygen deriving (Show, Eq, Enum)

data Way = Way
  { wIS    :: !IntcodeState
  , _wPath :: ![Dir]
  , _wStep :: !Int
  , _wPos  :: !Pos
  }

type SectionMap = AreaMap (Int, Cell)

type MonadBFS m =
  ( MonadState SectionMap m
  , MonadIO m
  )

main :: IO ()
main = do
  program <- Input.ints
  putStrLn "Step 1"
  area <- fillMap program
  display area
  print $ head $ filterOxygen area
  putStrLn "Step 2"
  let oxygenizedArea = fillOxygen area
  display oxygenizedArea
  print $ maximum $ filterOxygen oxygenizedArea
  where
    filterOxygen m = [ v | (_, (v, Oxygen)) <- Map.toList m ]
    display = Area.visualize $ \case
      Nothing          -> ' '
      Just (_, Wall)   -> '#'
      Just (_, Empty)  -> '.'
      Just (_, Oxygen) -> 'O'

runEnv :: Env a -> Dir -> (a, Maybe Cell)
runEnv (Env e) input = fmap toEnum . esOutput <$> runState e EnvState
  { esInput  = fromEnum input + 1
  , esOutput = Nothing
  }

step :: MonadIO m => IntcodeState -> Dir -> m (Maybe Cell, IntcodeState)
step s dir =
  case runEnv (Intcode.rerunIntcode s) dir of
    ((Left err, _), _) -> failWith $ show err
    ((_, x),        y) -> pure (y, x)

tryWay
  :: MonadBFS m
  => Way -> m (Maybe IntcodeState)
tryWay (Way _ [] _ _)            = failWith "No way!"
tryWay (Way is (d:_) n pos) =
  gets (Map.lookup pos) >>= \case
    Just (l, _) | l <= n -> pure Nothing
    _                       ->
      step is d >>= \case
        (Nothing, _)  -> failWith "Unexpected halt!"
        (Just c, is') -> do
          modify $ Map.insert pos (n, c)
          case c of
            Wall -> pure Nothing
            _    -> pure $ Just is'

fillMap :: MonadIO m => Intcode -> m SectionMap
fillMap program =
  go (fork [Way start [] 0 (0, 0)]) `execStateT` Map.singleton (0, 0) (0, Empty)
  where
    start = Intcode.initState program
    go [] = pure ()
    go ps = check [] ps >>= go . fork
    check acc []     = pure acc
    check acc (w:ws) = tryWay w >>= \case
      Just is -> check (w { wIS = is } : acc) ws
      Nothing -> check                   acc  ws

fillOxygen :: SectionMap -> SectionMap
fillOxygen = go . Map.map retag
  where
    retag (_, x) = (0, x)
    go m
      | length patch == 0 = m
      | otherwise         = go $ Map.union patch m
      where
        patch = Map.fromList
          [ (pos, (minimum vs + 1, Oxygen))
          | (pos, (_, Empty)) <- Map.toList m
          , let vs = [ v | (_, _, (v, Oxygen)) <- Area.neibs pos m ]
          , not (null vs)
          ]

fork :: [Way] -> [Way]
fork ways =
  [ Way is (d:path) (s + 1) (Area.move d pos)
  | Way is path s pos <- ways
  , d <- dirs
  ]

failWith :: MonadIO m => String -> m a
failWith = liftIO . throwIO . EvalError
