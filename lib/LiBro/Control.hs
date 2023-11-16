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
  _ <- takeMVar blocking
  return ()

-- |  Try to store shared state data. Expects the given blocking MVar
--    to be empty. Iff not, returns 'False'.
saveData :: Config -> MVar Blocking -> MVar LiBroData -> IO Bool
saveData config blocking libroData = do
  isBlocked <- not <$> isEmptyMVar blocking
  if isBlocked
    then return False
    else do
      putMVar blocking Writing
      storeData config =<< readMVar libroData
      _ <- takeMVar blocking
      return True
