-- |  Basic definitions.
module LiBro.Base where

import LiBro.Config
import Control.Monad.Reader

-- |  Internal monad for 'Config'ured libro effects.
newtype LiBro a = LiBro
  { unLiBro :: ReaderT Config IO a
  } deriving  ( Functor
              , Applicative
              , Monad
              , MonadReader Config
              , MonadFail
              , MonadIO
              )

-- |  Run a 'Config'ured libro effect in 'IO'.
runLiBro :: Config -> LiBro a -> IO a
runLiBro config = flip runReaderT config . unLiBro
