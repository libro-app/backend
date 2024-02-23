module LiBro.WebService where

import LiBro.Control
import LiBro.Data
import qualified Data.Map as M
import Data.Tree
import Data.Aeson
import Data.Proxy
import Servant
import Control.Monad.Reader
import GHC.Generics

newtype PersonIDs = PersonIDs {personIDs  :: [Int]} deriving Generic
newtype TaskIDs   = TaskIDs   {taskIDs    :: [Int]} deriving Generic
instance ToJSON PersonIDs
instance ToJSON TaskIDs

type LiBroHandler = ReaderT LiBroState Handler

runAction :: Action a -> LiBroHandler a
runAction action = ask >>= liftIO . runReaderT action

type LiBroAPI = "person"                        :> Get '[JSON]  PersonIDs
          :<|>  "person"  :> Capture "pid" Int  :> Get '[JSON]  Person
          :<|>  "task"                          :> Get '[JSON]  TaskIDs

libroServer :: ServerT LiBroAPI LiBroHandler
libroServer =     hPersonIDs
            :<|>  hPersonDetails
            :<|>  hTaskIDs
  where
        hPersonIDs :: LiBroHandler PersonIDs
        hPersonIDs = do
          ps <- persons <$> runAction lsData
          return $ PersonIDs (M.keys ps)

        hPersonDetails :: Int -> LiBroHandler Person
        hPersonDetails pId = do
          ps <- persons <$> runAction lsData
          case M.lookup pId ps of
            Just p  -> return p
            Nothing -> throwError err404 {errBody = "Person not found"}

        hTaskIDs :: LiBroHandler TaskIDs
        hTaskIDs = do
          ts <- tasks <$> runAction lsData
          return $ TaskIDs (tid . rootLabel <$> ts)

libroApi :: Proxy LiBroAPI
libroApi = Proxy

libro :: LiBroState -> Application
libro initState =
  let server = hoistServer libroApi (`runReaderT` initState) libroServer
  in  serve libroApi server
