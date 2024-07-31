-- |  Controlling the LiBro data flow.
module LiBro.Control where

import LiBro.Base
import LiBro.Config
import LiBro.Data
import LiBro.Data.Storage
import Control.Concurrent
import Control.Monad.Reader

-- |  Represents a blocking action because the system is loading
--    or saving data.
data Blocking
  = Reading
  | Writing
  deriving (Eq, Show)

-- |  Initially load data and put it into the shared state.
--    Expects the given 'MVar' to be empty.
initData :: MVar Blocking -> MVar LiBroData -> LiBro ()
initData blocking libroData = do
  liftIO $ putMVar blocking Reading
  ld <- loadData
  _ <- liftIO $ putMVar libroData ld
  _ <- liftIO $ takeMVar blocking
  return ()

-- |  Try to store shared state data. Expects the given blocking 'MVar'
--    to be empty. Iff not, returns 'False'.
saveData :: MVar Blocking -> MVar LiBroData -> LiBro Bool
saveData blocking libroData = do
  isBlocked <- not <$> liftIO (isEmptyMVar blocking)
  if isBlocked
    then return False
    else do
      liftIO $ putMVar blocking Writing
      storeData =<< liftIO (readMVar libroData)
      _ <- liftIO $ takeMVar blocking
      return True
