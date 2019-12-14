module Parsing
  ( module Data.Char
  , module CNE
  , module MP
  , Parser
  , parseIt
  , parseLines
  ) where

import Data.Char
import Data.Bifunctor (first)
import Control.Applicative.Combinators.NonEmpty as CNE hiding (some)
import Text.Megaparsec as MP hiding (sepBy1, someTill, sepEndBy1, endBy1)
import Text.Megaparsec.Char as MP
import Text.Megaparsec.Char.Lexer as MP hiding (space)
import Data.Void

type Parser a = Parsec Void String a

parseIt :: Parser a -> String -> Either String a
parseIt p = first errorBundlePretty . runParser p ""

parseLines :: Parser a -> String -> Either String [a]
parseLines p = mapM (parseIt p) . lines
