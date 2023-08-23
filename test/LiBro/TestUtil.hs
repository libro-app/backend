module LiBro.TestUtil where

import LiBro.Data
import LiBro.Data.SafeText
import LiBro.Util
import Data.List
import qualified Data.Map as M
import Data.Tree
import Data.Bifunctor
import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans.Class

-- Person generator with a given person ID.
genPerson :: Int -> Gen Person
genPerson pid = Person  <$> return pid
                        <*> arbitrary
                        <*> arbitrary

-- Unique person lists generator
genPersons :: Gen Persons
genPersons = do
  persons <- mapM genPerson =<< arbitrary `suchThat` unique
  return $ personMap persons
  where unique = (==) <*> nub

-- Task generator with task ID and _allowed_ persons
genTask :: Persons -> Int -> Gen Task
genTask ps tid = Task <$> return tid
                      <*> arbitrary
                      <*> arbitrary
                      <*> sublistOf (M.elems ps)

-- |  A QuickCheck 'Gen' variant that can count
--    (helpful for unique IDs).
type CountingGen = CountingT Gen

scaledListOf :: (Int -> Int) -> CountingGen a -> CountingGen [a]
scaledListOf s g = do
  oldSize <- lift $ getSize
  newSize <- lift $ chooseInt (0, s oldSize)
  replicateM newSize g

countingGenTaskTree :: Persons -> CountingGen (Tree Task)
countingGenTaskTree ps = do
  tid       <- next
  task      <- lift $ genTask ps tid
  children  <- scaledListOf (`div` 50) $ countingGenTaskTree ps
  return $ Node task children

-- |  Task tree generator with unique IDs and _allowed_ persons
genTaskTree :: Persons -> Gen (Tree Task)
genTaskTree ps = runCountingT (countingGenTaskTree ps) =<< arbitrary

-- |  Task forest generator with unique IDs and _allowed_ persons
genTaskForest :: Persons -> Gen Tasks
genTaskForest ps = runCountingT gtf =<< arbitrary
  where gtf = scaledListOf (`div` 2) $ countingGenTaskTree ps

-- |  Generate a full data setting with non-empty persons
--    with unique person ids and tasks with valid assignees
genPersonsTasks :: Gen LiBroData
genPersonsTasks = do
  ps <- genPersons `suchThat` (not.null)
  ts <- genTaskForest ps
  return $ LBS ps ts
