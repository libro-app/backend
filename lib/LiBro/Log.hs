module LiBro.Log where

import Text.Printf
import Data.Time.Clock

data LogLevel   = INFO | WARNING | ERROR | FATAL deriving (Eq, Ord, Show)
type LogSource  = String
type LogMessage = String
data Log = Log
  { time    :: UTCTime
  , level   :: LogLevel
  , source  :: LogSource
  , message :: LogMessage
  }

instance Show Log where
  show (Log t l s m) = printf "%s [%s] (%s): %s" (show l) (show t) s m
