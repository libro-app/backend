-- |  Controlling the LiBro data flow.
module LiBro.Control where

import LiBro.Config
import LiBro.Data
import LiBro.Data.Storage
import Control.Concurrent

-- |  Represents a blocking action because the system is loading
--    or saving data.
data Blocking
  = Reading
  | Writing
  deriving (Eq, Show)

-- |  Initially load data and put it into the shared state.
--    Expects the given 'MVar' to be empty.
initData :: Config -> MVar Blocking -> MVar LiBroData -> IO ()
initData config blocking libroData = do
  putMVar blocking Reading
  putMVar libroData =<< loadData config
  takeMVar blocking
  return ()
