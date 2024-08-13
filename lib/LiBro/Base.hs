-- |  Basic definitions.
module LiBro.Base where

import LiBro.Config
import LiBro.Util as Util
import Data.Csv
import System.Directory as Dir
import Control.Monad.Reader
import Control.Concurrent as Conc

-- |  Type class for 'Config'ured libro effects.
class (Monad m, MonadFail m) => MonadLiBro m where

  readConfig :: (Config -> a) -> m a

  doesFileExist :: FilePath -> m Bool

  loadFromXlsx  :: FromNamedRecord a => FilePath -> m (Either String [a])
  storeAsXlsx   :: (DefaultOrdered a, ToNamedRecord a) => FilePath -> [a] -> m ()

  readMVar    :: MVar a -> m a
  putMVar     :: MVar a -> a -> m ()
  takeMVar    :: MVar a -> m a
  isEmptyMVar :: MVar a -> m Bool

-- |  The default configured 'LiBro' effect using 'IO'.
newtype LiBroIO a = LiBro
  { unLiBro :: ReaderT Config IO a
  } deriving  ( Functor, Applicative, Monad
              , MonadFail, MonadIO
              , MonadReader Config
              )

instance MonadLiBro LiBroIO where
  readConfig        = asks
  doesFileExist fp  = liftIO $ Dir.doesFileExist fp
  loadFromXlsx fp   = liftIO $ Util.loadFromXlsx fp
  storeAsXlsx fp d  = liftIO $ Util.storeAsXlsx fp d
  readMVar mv       = liftIO $ Conc.readMVar mv
  putMVar mv d      = liftIO $ Conc.putMVar mv d
  takeMVar mv       = liftIO $ Conc.takeMVar mv
  isEmptyMVar mv    = liftIO $ Conc.isEmptyMVar mv

-- |  Run a 'Config'ured libro effect in 'IO'.
runLiBroIO :: Config -> LiBroIO a -> IO a
runLiBroIO config = flip runReaderT config . unLiBro
