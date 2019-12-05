module Input
  ( ints
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

ints :: IO [Int]
ints = parse <$> TIO.getLine
  where
    parse :: T.Text -> [Int]
    parse = fromMaybe [] . mapM (readMaybe . T.unpack). T.split (== ',')
