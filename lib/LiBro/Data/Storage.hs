-- |  LiBro data transformations for storage
module LiBro.Data.Storage where

import LiBro.Data
import LiBro.Util
import Data.Text (Text)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import qualified Data.ByteString.Char8 as B
import GHC.Generics

newtype IdList = IdList { ids :: [Int] } deriving Eq

instance Show IdList where
  show = unwords . map show . ids
instance FromField IdList where
  parseField = fmap (IdList . map read . words) . parseField
instance ToField IdList where
  toField = B.pack . show

data TaskRecord = TaskRecord
  { trid          :: Int
  , parentTid     :: Maybe Int
  , tTitle        :: Text
  , tDescription  :: Text
  , tAssignees    :: IdList
  } deriving (Eq, Show, Generic)

instance DefaultOrdered TaskRecord
instance FromNamedRecord TaskRecord
instance ToNamedRecord TaskRecord

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
