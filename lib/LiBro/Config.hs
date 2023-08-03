-- |  Configuration (file) handling.
module LiBro.Config where

import Data.Default
import Data.Ini.Config
import Data.Text
import qualified Data.Text.IO as TIO
import System.IO

-- |  Configuration of storage details.
data StorageConfig = Storage
  { directory     :: FilePath
  , personFile    :: String
  , tasksFile     :: String
  , trackingFile  :: String
  } deriving (Eq, Show)

instance Default StorageConfig where
  def = Storage "data-storage" "persons.xlsx" "tasks.xlsx" "tracking.xlsx"

-- |  Global settings.
data Config = Config
  { storage :: StorageConfig
  } deriving (Eq, Show)

instance Default Config where
  def = Config def

-- |  Parses a 'Config' value from a given 'Text'
--    or gives a parsing error message.
parseConfig :: Text -> Either String Config
parseConfig = flip parseIniFile $ do
  st <- section "storage" $
    Storage <$> fieldOf "directory"     string
            <*> fieldOf "person-file"   string
            <*> fieldOf "tasks-file"    string
            <*> fieldOf "tracking-file" string
  return $ Config st

-- |  Reads a 'Config' value from @config.ini@.
--    Prints parsing error messages to @STDERR@ when failing.
readConfig :: IO (Maybe Config)
readConfig = readConfigFrom "config.ini"

-- |  Reads a 'Config' value from the given file path.
--    Prints parsing error messages to @STDERR@ when failing.
readConfigFrom :: FilePath -> IO (Maybe Config)
readConfigFrom fp = do
  content <- TIO.readFile fp
  case parseConfig content of
    Right config  -> return (Just config)
    Left errorMsg -> do
      hPutStrLn stderr $ "Error parsing '" ++ fp ++ ":\n" ++ errorMsg
      return Nothing
