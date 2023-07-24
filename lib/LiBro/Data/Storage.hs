-- |  LiBro data transformations for storage
module LiBro.Data.Storage where

import Data.Text (Text)
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
  { tid           :: Int
  , parentTid     :: Maybe Int
  , tTitle        :: Text
  , tDescription  :: Text
  , tAssignees    :: IdList
  } deriving (Eq, Show, Generic)

instance DefaultOrdered TaskRecord
instance FromNamedRecord TaskRecord
instance ToNamedRecord TaskRecord
