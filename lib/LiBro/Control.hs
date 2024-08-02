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

-- |  Shared libro system state to access data any time.
data LiBroState = LiBroState
  { config      :: Config
  , mvBlocking  :: MVar Blocking
  , mvData      :: MVar LiBroData
  }

-- |  Initialization of a 'LiBroState'.
initLiBroState :: LiBro LiBroState
initLiBroState = do
  mvb <- liftIO newEmptyMVar
  mvd <- liftIO newEmptyMVar
  initData mvb mvd
  cfg <- ask
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
  lift $ runLiBro cfg $ initData mvb mvd

-- |  'saveData' action.
lsSaveData :: Action Bool
lsSaveData = do
  cfg <- asks config
  mvb <- asks mvBlocking
  mvd <- asks mvData
  lift $ runLiBro cfg $ saveData mvb mvd
