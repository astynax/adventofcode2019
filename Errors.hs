module Errors
  ( EvalError(..)
  , failWith
  ) where

import Control.Monad.IO.Class
import Control.Exception

newtype EvalError = EvalError String deriving Show

instance Exception EvalError

failWith :: MonadIO m => String -> m a
failWith = liftIO . throwIO . EvalError
