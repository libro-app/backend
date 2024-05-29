-- |  Controlling the LiBro data flow.
module LiBro.Control where

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
initData :: Config -> MVar Blocking -> MVar LiBroData -> IO ()
initData cfg blocking libroData = do
  putMVar blocking Reading
  putMVar libroData =<< loadData cfg
  _ <- takeMVar blocking
  return ()

-- |  Try to store shared state data. Expects the given blocking MVar
--    to be empty. Iff not, returns 'False'.
saveData :: Config -> MVar Blocking -> MVar LiBroData -> IO Bool
saveData cfg blocking libroData = do
  isBlocked <- not <$> isEmptyMVar blocking
  if isBlocked
    then return False
    else do
      putMVar blocking Writing
      storeData cfg =<< readMVar libroData
      _ <- takeMVar blocking
      return True

-- |  Shared libro system state to access data any time.
data LiBroState = LiBroState
  { config      :: Config
  , mvBlocking  :: MVar Blocking
  , mvData      :: MVar LiBroData
  }

-- |  Initialization of a 'LiBroState'.
initLiBroState :: Config -> IO LiBroState
initLiBroState cfg = do
  mvb <- newEmptyMVar
  mvd <- newEmptyMVar
  initData cfg mvb mvd
  return $ LiBroState cfg mvb mvd

-- |  Type alias for actions holding a 'LiBroState' inside 'ReaderT'.
type Action = ReaderT LiBroState IO

-- |  'Config' accessor action.
lsConfig :: Action Config
lsConfig = asks config

-- |  Checks whether the system is blocked
--    and by what type of 'Blocking' action.
lsBlockedBy :: Action (Maybe Blocking)
lsBlockedBy = do
  mvb <- asks mvBlocking
  lift $ tryTakeMVar mvb

-- |  'LiBroData' accessor action.
lsData :: Action LiBroData
lsData = do
  mvd <- asks mvData
  lift $ readMVar mvd

-- |  'initData' action.
lsInitData :: Action ()
lsInitData = do
  cfg <- asks config
  mvb <- asks mvBlocking
  mvd <- asks mvData
  lift $ initData cfg mvb mvd

-- |  'saveData' action.
lsSaveData :: Action Bool
lsSaveData = do
  cfg <- asks config
  mvb <- asks mvBlocking
  mvd <- asks mvData
  lift $ saveData cfg mvb mvd
