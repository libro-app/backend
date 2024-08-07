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

import LiBro.Base
import LiBro.Config
import LiBro.Data
import LiBro.Data.SafeText
import LiBro.Util
import Data.Function
import Data.Map ((!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import qualified Data.ByteString.Char8 as B
import Control.Monad.Reader
import GHC.Generics
import System.FilePath
import System.Directory

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
          let tr  = toTaskRecord parent t
              trs = storeTasks' (Just $ tid t) <$> ts
          in  tr : concat trs
        toTaskRecord p t = TaskRecord
          { trid          = tid t
          , parentTid     = p
          , tTitle        = title t
          , tDescription  = description t
          , tAssignees    = IdList (pid <$> assignees t)
          }

-- |  Load 'Task's from 'TaskRecord's. Needs to lookup 'Person's.
taskRecordsToTasks :: Persons -> [TaskRecord] -> Tasks
taskRecordsToTasks pmap trs =
  let tmap        = M.fromList $ map ((,) =<< trid) trs
      parentList  = map ((,) <$> trid <*> parentTid) trs
      idForest    = readForest parentList
  in  map (fmap $ fromRecord . (tmap !)) idForest
  where fromRecord tr = Task
          { tid         = trid tr
          , title       = tTitle tr
          , description = tDescription tr
          , assignees   = (pmap !) <$> ids (tAssignees tr)
          }

-- |  Store 'Person's at the configured storage space
storePersons :: Persons -> LiBro ()
storePersons pmap = do
  sconf <- asks storage
  let fp = directory sconf </> personFile sconf
  liftIO $ storeAsXlsx fp $ M.elems pmap

-- |  Load a list of 'Person's from the configured storage space.
--    Returns empty data if no input file was found.
loadPersons :: LiBro Persons
loadPersons = do
  sconf <- asks storage
  let fp = directory sconf </> personFile sconf
  exists <- liftIO $ doesFileExist fp
  if not exists then return M.empty
    else do
      Right prs <- liftIO $ loadFromXlsx fp
      return $ personMap prs

-- |  Store 'Tasks' at the configured storage space.
storeTasks :: Tasks -> LiBro ()
storeTasks ts = do
  sconf <- asks storage
  let fp = directory sconf </> tasksFile sconf
  liftIO $ storeAsXlsx fp $ tasksToTaskRecords ts

-- |  Load 'Tasks' from the configured storage space.
--    Needs an additional 'Data.Map.Map' to find 'Person's for given
--    person ids ('Int'). Returns empty data if no input file was found.
loadTasks :: Persons -> LiBro Tasks
loadTasks pmap = do
  sconf <- asks storage
  let fp = directory sconf </> tasksFile sconf
  exists <- liftIO $ doesFileExist fp
  if not exists then return []
    else do
      Right records <- liftIO $ loadFromXlsx fp
      return $ taskRecordsToTasks pmap records

-- |  Store a complete dataset at the configured file system
--    locations.
storeData :: LiBroData -> LiBro ()
storeData ld = do
  storePersons  $ persons ld
  storeTasks    $ tasks ld

-- |  Load a complete dataset from the configured file system
--    locations. Returns empty data if no input files were found.
loadData :: LiBro LiBroData
loadData = do
  pmap <- loadPersons
  ts   <- loadTasks pmap
  return $ LBS pmap ts
