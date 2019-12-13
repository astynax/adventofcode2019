module Input
  ( ints
  , intsFile
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

ints :: IO [Int]
ints = parse <$> TIO.getLine

intsFile :: FilePath -> IO [Int]
intsFile = fmap parse . TIO.readFile

parse :: T.Text -> [Int]
parse = fromMaybe [] . mapM (readMaybe . T.unpack). T.split (== ',')
