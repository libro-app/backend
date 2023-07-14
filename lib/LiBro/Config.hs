-- |  Configuration (file) handling
module LiBro.Config where

import Data.Default
import Data.Ini.Config
import Data.Text

data StorageConfig = Storage
  { directory     :: FilePath
  , tasksFile     :: String
  , trackingFile  :: String
  } deriving (Eq, Show)

instance Default StorageConfig where
  def = Storage "data-storage" "tasks.csv" "tracking.csv"

data Config = Config
  { storage :: StorageConfig
  } deriving (Eq, Show)

instance Default Config where
  def = Config def

parseConfig :: Text -> Either String Config
parseConfig = flip parseIniFile $ do
  st <- section "storage" $
    Storage <$> fieldOf "directory"     string
            <*> fieldOf "tasks-file"    string
            <*> fieldOf "tracking-file" string
  return $ Config st

