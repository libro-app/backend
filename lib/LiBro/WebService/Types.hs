module LiBro.WebService.Types where

import Data.Aeson
import GHC.Generics

newtype PersonIDs = PersonIDs {personIDs :: [Int]} deriving Generic
instance ToJSON PersonIDs
