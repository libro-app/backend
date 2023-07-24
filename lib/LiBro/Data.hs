-- |  Little Brother data representation
module LiBro.Data where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Graph
import Data.Function
import Data.Aeson
import GHC.Generics

data Person = Person
  { pid   :: Int
  , name  :: Text
  , email :: Text
  } deriving (Show, Generic)

instance Eq Person where (==) = (==) `on` pid
instance ToJSON Person
instance FromJSON Person

data Task = Task
  { tid         :: Int
  , title       :: Text
  , description :: Text
  , assignees   :: [Person]
  } deriving (Show, Generic)

instance Eq Task where (==) = (==) `on` tid
instance ToJSON Task
instance FromJSON Task

-- |  The primary data type for tasks, 'Tree's of 'Task'.
type Tasks = Forest Task
