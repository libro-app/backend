-- |  LiBro data transformations for storage
module LiBro.Data.Storage where

import LiBro.Config
import LiBro.Data
import LiBro.Data.SafeText
import LiBro.Util
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Function
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import GHC.Generics
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Process

-- |  Helper function to create a person 'Map' from 'Int' to 'Person'
--    from a given 'Person' list. Useful for 'Task' reading functions.
personMap :: [Person] -> Map Int Person
personMap = M.fromList . map ((,) =<< pid)

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
instance FromRecord TaskRecord
instance ToRecord TaskRecord
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

-- |  Load 'Task's from 'TaskRecord's. Needs an additional 'Map'
--    to find 'Person's for given person ids ('Int').
taskRecordsToTasks :: Map Int Person -> [TaskRecord] -> Tasks
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

-- |  Store given CSV data as Excel spreadsheet. Stores the given
--    CSV data to a temporary file that is deleted afterwards and
--    uses the (hopefully) installed @libreoffice@ to convert. The
--    CSV data is stored in the (first) worksheet named @export@.
storeCSVasXLSX :: FilePath -> ByteString -> IO ()
storeCSVasXLSX fp csv = do
  withSystemTempDirectory "excel-export" $ \tdir -> do
    let csvFile   = tdir </> "export.csv"
    let xlsxFile  = tdir </> "export.xlsx"
    BS.writeFile csvFile csv
    callCommand $ unwords
      [ "libreoffice --calc --convert-to xlsx"
      , "--outdir", tdir, csvFile
      , "> /dev/null"
      ]
    renameFile xlsxFile fp

-- |  Load CSV from an Excel spreadsheet using @libreoffice@.
loadCSVfromXLSX :: FilePath -> IO ByteString
loadCSVfromXLSX fp = do
  withSystemTempDirectory "excel-import" $ \tdir -> do
    let csvFile   = tdir </> "import.csv"
    let xlsxFile  = tdir </> "import.xlsx"
    copyFile fp xlsxFile
    callCommand $ unwords
      [ "libreoffice --calc --convert-to csv"
      , "--outdir", tdir, xlsxFile
      , "> /dev/null"
      ]
    BS.readFile csvFile

-- |  Store a list of 'Person's at the configured storage space
--    via 'Config'.
storePersons :: Config -> [Person] -> IO ()
storePersons conf persons = do
  let sconf = storage conf
      fp    = directory sconf </> personFile sconf
      csv   = encodeDefaultOrderedByName persons
  storeCSVasXLSX fp csv

-- |  Load a list of 'Person's from the configured storage space
--    via 'Config'.
loadPersons :: Config -> IO [Person]
loadPersons conf = do
  let sconf = storage conf
      fp    = directory sconf </> personFile sconf
  csv <- loadCSVfromXLSX fp
  let (Right records) = decode HasHeader csv
  return $ V.toList records

-- |  Store 'Tasks' at the configured storage space via 'Config'.
storeTasks :: Config -> Tasks -> IO ()
storeTasks conf tasks = do
  let sconf = storage conf
      fp    = directory sconf </> tasksFile sconf
      trs   = tasksToTaskRecords tasks
      csv   = encodeDefaultOrderedByName trs
  storeCSVasXLSX fp csv

-- |  Load 'Tasks' from the configured storage space via 'Config'.
--    Needs an additional 'Map' to find 'Person's for given person
--    ids ('Int').
loadTasks :: Config -> Map Int Person -> IO Tasks
loadTasks conf persons = do
  let sconf = storage conf
      fp    = directory sconf </> tasksFile sconf
  csv <- loadCSVfromXLSX fp
  let (Right records) = V.toList <$> decode HasHeader csv
  return $ taskRecordsToTasks persons records

-- |  Store a complete dataset at the 'Config'ured file system
--    locations.
storeData :: Config -> ([Person], Tasks) -> IO ()
storeData conf (persons, tasks) = do
  storePersons  conf persons
  storeTasks    conf tasks

-- |  Load a complete dataset from the 'Config'ured file system
--    locations.
loadData :: Config -> IO ([Person], Tasks)
loadData conf = do
  persons <- loadPersons  conf
  tasks   <- loadTasks    conf (personMap persons)
  return (persons, tasks)
