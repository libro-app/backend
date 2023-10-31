-- |  LiBro data transformations for storage
module LiBro.Data.Storage
  (
  -- * Handling of multiple IDs in a single value
    IdList(..)
  , idListToStr
  , strToIdList
  -- * Storable task records
  , TaskRecord(..)
  , tasksToTaskRecords
  , taskRecordsToTasks
  -- * Top level data handling
  , storePersons
  , loadPersons
  , storeTasks
  , loadTasks
  , storeData
  , loadData
  ) where

import LiBro.Config
import LiBro.Data
import LiBro.Data.SafeText
import LiBro.Util
import Data.Function
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import qualified Data.ByteString.Char8 as B
import GHC.Generics
import System.FilePath

-- |  A thin wrapper around lists of 'Int' with a simple
--    (space-separated) 'String' representation.
newtype IdList = IdList { ids :: [Int] } deriving (Eq, Generic)

-- |  Simple 'String' representation of an 'IdList': space-separated numbers.
idListToStr :: IdList -> String
idListToStr = unwords . map show . ids

-- |  Reads space-separated 'Int's to an 'IdList'.
strToIdList :: String -> IdList
strToIdList = IdList . map read . words

instance Show IdList where
  show = idListToStr
instance Read IdList where
  readsPrec _ str = [(strToIdList str, "")]
instance FromField IdList where
  parseField = fmap read . parseField
instance ToField IdList where
  toField = B.pack . show

-- |  A data type specialized to store 'Task' information in tables.
data TaskRecord = TaskRecord
  { trid          :: Int
  , parentTid     :: Maybe Int
  , tTitle        :: SafeText
  , tDescription  :: SafeText
  , tAssignees    :: IdList
  } deriving (Show, Generic)

instance Eq TaskRecord where (==) = (==) `on` trid
instance DefaultOrdered TaskRecord
instance FromNamedRecord TaskRecord
instance ToNamedRecord TaskRecord

-- |  Store 'Task's using 'TaskRecord's.
tasksToTaskRecords :: Tasks -> [TaskRecord]
tasksToTaskRecords = concatMap (storeTasks' Nothing)
  where storeTasks' parent (Node t ts) =
          let tr  = toRecord parent t
              trs = storeTasks' (Just $ tid t) <$> ts
          in  tr : concat trs
        toRecord p t = TaskRecord
          { trid          = tid t
          , parentTid     = p
          , tTitle        = title t
          , tDescription  = description t
          , tAssignees    = IdList (pid <$> assignees t)
          }

-- |  Load 'Task's from 'TaskRecord's. Needs to lookup 'Person's.
taskRecordsToTasks :: Persons -> [TaskRecord] -> Tasks
taskRecordsToTasks persons trs =
  let tasks       = M.fromList $ map ((,) =<< trid) trs
      parentList  = map ((,) <$> trid <*> parentTid) trs
      idForest    = readForest parentList
  in  map (fmap $ fromRecord . (tasks !)) idForest
  where fromRecord tr = Task
          { tid         = trid tr
          , title       = tTitle tr
          , description = tDescription tr
          , assignees   = (persons !) <$> ids (tAssignees tr)
          }

-- |  Store 'Person's at the configured storage space
--    via 'Config'.
storePersons :: Config -> Persons -> IO ()
storePersons conf persons = do
  let sconf = storage conf
      fp    = directory sconf </> personFile sconf
  storeAsXlsx fp $ M.elems persons

-- |  Load a list of 'Person's from the configured storage space
--    via 'Config'.
loadPersons :: Config -> IO Persons
loadPersons conf = do
  let sconf = storage conf
      fp    = directory sconf </> personFile sconf
  Right prs <- loadFromXlsx fp
  return $ personMap prs

-- |  Store 'Tasks' at the configured storage space via 'Config'.
storeTasks :: Config -> Tasks -> IO ()
storeTasks conf tasks = do
  let sconf = storage conf
      fp    = directory sconf </> tasksFile sconf
  storeAsXlsx fp $ tasksToTaskRecords tasks

-- |  Load 'Tasks' from the configured storage space via 'Config'.
--    Needs an additional 'Map' to find 'Person's for given person
--    ids ('Int').
loadTasks :: Config -> Persons -> IO Tasks
loadTasks conf persons = do
  let sconf = storage conf
      fp    = directory sconf </> tasksFile sconf
  Right records <- loadFromXlsx fp
  return $ taskRecordsToTasks persons records

-- |  Store a complete dataset at the 'Config'ured file system
--    locations.
storeData :: Config -> LiBroData -> IO ()
storeData conf ld = do
  storePersons  conf (persons ld)
  storeTasks    conf (tasks ld)

-- |  Load a complete dataset from the 'Config'ured file system
--    locations.
loadData :: Config -> IO LiBroData
loadData conf = do
  persons <- loadPersons  conf
  tasks   <- loadTasks    conf persons
  return $ LBS persons tasks
