-- |  Controlling the LiBro data flow.
module LiBro.Control where

import LiBro.Base
import LiBro.Data
import LiBro.Data.Storage
import Control.Concurrent (MVar)

-- |  Represents a blocking action because the system is loading
--    or saving data.
data Blocking
  = Reading
  | Writing
  deriving (Eq, Show)

-- |  Initially load data and put it into the shared state.
--    Expects the given 'MVar' to be empty.
initData :: MonadLiBro m => MVar Blocking -> MVar LiBroData -> m ()
initData blocking libroData = do
  putMVar blocking Reading
  ld <- loadData
  _ <- putMVar libroData ld
  _ <- takeMVar blocking
  return ()

-- |  Try to store shared state data. Expects the given blocking 'MVar'
--    to be empty. Iff not, returns 'False'.
saveData :: MonadLiBro m => MVar Blocking -> MVar LiBroData -> m Bool
saveData blocking libroData = do
  isBlocked <- not <$> isEmptyMVar blocking
  if isBlocked
    then return False
    else do
      putMVar blocking Writing
      storeData =<< readMVar libroData
      _ <- takeMVar blocking
      return True
