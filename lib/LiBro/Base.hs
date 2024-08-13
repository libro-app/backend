-- |  Basic definitions.
module LiBro.Base where

import LiBro.Config
import LiBro.Log
import LiBro.Util as Util
import Data.Csv
import Data.Time.Clock
import System.Directory as Dir
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent as Conc

-- |  Type class for 'Config'ured libro effects.
class (Monad m, MonadFail m) => MonadLiBro m where

  readConfig :: (Config -> a) -> m a

  logInfo, logWarning, logError, logFatal :: LogSource -> LogMessage -> m ()
  logInfo     = addLog INFO
  logWarning  = addLog WARNING
  logError    = addLog ERROR
  logFatal    = addLog FATAL
  addLog    :: LogLevel -> LogSource -> LogMessage -> m ()

  doesFileExist :: FilePath -> m Bool

  loadFromXlsx  :: FromNamedRecord a => FilePath -> m (Either String [a])
  storeAsXlsx   :: (DefaultOrdered a, ToNamedRecord a) => FilePath -> [a] -> m ()

  readMVar    :: MVar a -> m a
  putMVar     :: MVar a -> a -> m ()
  takeMVar    :: MVar a -> m a
  isEmptyMVar :: MVar a -> m Bool

-- |  The default configured 'LiBro' effect using 'IO'.
newtype LiBroIO a = LiBro
  { unLiBro :: ReaderT Config (WriterT [Log] IO) a
  } deriving  ( Functor, Applicative, Monad, MonadFail
              , MonadReader Config
              , MonadWriter [Log]
              , MonadIO
              )

instance MonadLiBro LiBroIO where
  readConfig        = asks
  addLog l s m      = do {now <- liftIO getCurrentTime; tell [Log now l s m]}
  doesFileExist fp  = liftIO $ Dir.doesFileExist fp
  loadFromXlsx fp   = liftIO $ Util.loadFromXlsx fp
  storeAsXlsx fp d  = liftIO $ Util.storeAsXlsx fp d
  readMVar mv       = liftIO $ Conc.readMVar mv
  putMVar mv d      = liftIO $ Conc.putMVar mv d
  takeMVar mv       = liftIO $ Conc.takeMVar mv
  isEmptyMVar mv    = liftIO $ Conc.isEmptyMVar mv

-- |  Run a 'Config'ured libro effect in 'IO'.
runLiBroIO :: Config -> LiBroIO a -> IO a
runLiBroIO config action = do
  (result, logs) <- runLiBroIOLogs config action
  mapM_ print logs
  return result

-- |  Run a 'Config'ured libro effect in 'IO' with logs attached.
runLiBroIOLogs :: Config -> LiBroIO a -> IO (a, [Log])
runLiBroIOLogs config = runWriterT . flip runReaderT config . unLiBro
