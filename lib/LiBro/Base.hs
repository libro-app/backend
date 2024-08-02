-- |  Basic definitions.
module LiBro.Base where

import LiBro.Config
import Control.Monad.Reader

newtype LiBro a = LiBro
  { unLiBro :: ReaderT Config IO a
  } deriving  ( Functor
              , Applicative
              , Monad
              , MonadReader Config
              , MonadFail
              , MonadIO
              )

runLiBro :: Config -> LiBro a -> IO a
runLiBro config = flip runReaderT config . unLiBro
