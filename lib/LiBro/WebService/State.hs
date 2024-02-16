module LiBro.WebService.State where

import LiBro.Config
import LiBro.Data
import LiBro.Control
import Control.Concurrent

data LiBroState = LiBroState
  { config      :: Config
  , mvBlocking  :: MVar Blocking
  , mvData      :: MVar LiBroData
  }

lsConfig :: LiBroState -> IO Config
lsConfig = return . config

lsData :: LiBroState -> IO LiBroData
lsData = readMVar . mvData

lsInit :: Config -> IO LiBroState
lsInit config = do
  mvb <- newEmptyMVar
  mvd <- newEmptyMVar
  initData config mvb mvd
  return $ LiBroState config mvb mvd
