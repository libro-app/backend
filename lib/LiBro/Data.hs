-- |  Little Brother data representation
module LiBro.Data where

import LiBro.Data.SafeText
import Data.Tree
import Data.Function
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson
import GHC.Generics
import Data.Csv

-- |  A person that is assigned to 'Task's.
data Person = Person
  { pid   :: Int
  , name  :: SafeText
  , email :: SafeText
  } deriving (Show, Generic)

instance Eq Person where (==) = (==) `on` pid
instance Ord Person where (<=) = (<=) `on` pid
instance ToJSON Person
instance FromJSON Person
instance FromNamedRecord Person
instance DefaultOrdered Person
instance ToNamedRecord Person

-- |  'Person' container as a lookup table with person ID index.
type Persons = Map Int Person

-- |  Helper function to create a persons container.
personMap :: [Person] -> Persons
personMap = M.fromList . map ((,) =<< pid)

-- |  Internal task representation.
data Task = Task
  { tid         :: Int
  , title       :: SafeText
  , description :: SafeText
  , assignees   :: [Person]
  } deriving (Show, Generic)

instance Eq Task where (==) = (==) `on` tid
instance Ord Task where (<=) = (<=) `on` tid
instance ToJSON Task
instance FromJSON Task

-- |  The primary data type for tasks, 'Tree's of 'Task'.
type Tasks = Forest Task

-- |  Find all 'Task's assigned to a given 'Person'.
assignedTasks :: Person -> Tasks -> [Task]
assignedTasks p = concatMap (filter ((p `elem`) . assignees) . flatten)

-- |  Complete LiBro state in one type
data LiBroData = LBS
  { persons :: Persons
  , tasks   :: Tasks
  } deriving (Eq, Show)
