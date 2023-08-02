-- |  LiBro data transformations for storage
module LiBro.Data.Storage where

import LiBro.Data
import LiBro.Util
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import qualified Data.ByteString.Char8 as B
import GHC.Generics
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Process

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
  , tTitle        :: Text
  , tDescription  :: Text
  , tAssignees    :: IdList
  } deriving (Eq, Show, Generic)

instance ToRecord TaskRecord
instance DefaultOrdered TaskRecord
instance FromNamedRecord TaskRecord
instance ToNamedRecord TaskRecord

-- |  Store 'Task's using 'TaskRecord's.
storeTasks :: Tasks -> [TaskRecord]
storeTasks = concatMap (storeTasks' Nothing)
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
loadTasks :: Map Int Person -> [TaskRecord] -> Tasks
loadTasks persons trs =
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
    callProcess "libreoffice"
      ["--calc", "--convert-to", "xlsx", "--outdir", tdir, csvFile]
    renameFile xlsxFile fp
