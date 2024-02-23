module LiBro.WebService where

import LiBro.Control
import LiBro.Data
import qualified Data.Map as M
import Data.Map ((!))
import Data.Aeson
import Data.Proxy
import Servant
import Control.Monad.Reader
import GHC.Generics

newtype PersonIDs = PersonIDs {personIDs :: [Int]} deriving Generic
instance ToJSON PersonIDs

type LiBroHandler = ReaderT LiBroState Handler

runAction :: Action a -> LiBroHandler a
runAction action = ask >>= liftIO . runReaderT action

type LiBroAPI = "person"                        :> Get '[JSON]  PersonIDs
          :<|>  "person"  :> Capture "pid" Int  :> Get '[JSON]  Person

libroServer :: ServerT LiBroAPI LiBroHandler
libroServer =     hPersonIDs
            :<|>  hPersonDetails
  where
        hPersonIDs :: LiBroHandler PersonIDs
        hPersonIDs = do
          ps <- persons <$> runAction lsData
          return $ PersonIDs (M.keys ps)

        hPersonDetails :: Int -> LiBroHandler Person
        hPersonDetails pId = do
          ps <- persons <$> runAction lsData
          return $ ps ! pId

libroApi :: Proxy LiBroAPI
libroApi = Proxy

libro :: LiBroState -> Application
libro initState =
  let server = hoistServer libroApi (`runReaderT` initState) libroServer
  in  serve libroApi server
