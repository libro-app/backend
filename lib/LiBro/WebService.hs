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

type LiBroHandler = ReaderT LiBroState Handler

runAction :: Action a -> LiBroHandler a
runAction action = ask >>= liftIO . runReaderT action

data PersonDetails = PersonDetails
  { person      :: Person
  , personTasks :: [Task]
  } deriving Generic
instance ToJSON PersonDetails

-- JSON-friendly rewrite of Forest/Tree, their ToJSON instance is weird
type TaskForest = [TaskTree]
data TaskTree   = TaskTree
  { task        :: Task
  , subTasks    :: TaskForest
  } deriving Generic
instance ToJSON TaskTree

type LiBroAPI =
        "person"                        :> Get '[JSON]  [Person]
  :<|>  "person"  :> Capture "pid" Int  :> Get '[JSON]  PersonDetails
  :<|>  "task"                          :> Get '[JSON]  [Task]
  :<|>  "task"    :> "tree"             :> Get '[JSON]  TaskForest

libroServer :: ServerT LiBroAPI LiBroHandler
libroServer =     hPersonList
            :<|>  hPersonDetails
            :<|>  hTaskTopLevelList
            :<|>  hTaskFullForest
  where
        hPersonList :: LiBroHandler [Person]
        hPersonList = M.elems . persons <$> runAction lsData

        hPersonDetails :: Int -> LiBroHandler PersonDetails
        hPersonDetails pId = do
          d <- runAction lsData
          case M.lookup pId (persons d) of
            Just p  ->  let ts = assignedTasks p (tasks d)
                        in  return $ PersonDetails p ts
            Nothing -> throwError err404 {errBody = "Person not found"}

        hTaskTopLevelList :: LiBroHandler [Task]
        hTaskTopLevelList = map rootLabel . tasks <$> runAction lsData

        hTaskFullForest :: LiBroHandler TaskForest
        hTaskFullForest = map convert . tasks <$> runAction lsData
          where convert (Node t sts) = TaskTree t (convert <$> sts)

libroApi :: Proxy LiBroAPI
libroApi = Proxy

libro :: LiBroState -> Application
libro initState =
  let server = hoistServer libroApi (`runReaderT` initState) libroServer
  in  serve libroApi server
