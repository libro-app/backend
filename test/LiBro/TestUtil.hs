module LiBro.TestUtil where

import LiBro.Data
import LiBro.Util
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Tree
import Data.Bifunctor
import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans.Class

instance Arbitrary Text where
  arbitrary = T.pack . getPrintableString <$> arbitrary

-- Person generator with a given person ID.
genPerson :: Int -> Gen Person
genPerson pid = Person  <$> return pid
                        <*> arbitrary
                        <*> arbitrary

-- Unique person lists generator
genPersons :: Gen [Person]
genPersons = mapM genPerson =<< arbitrary `suchThat` unique
  where unique = (==) <*> nub

-- Task generator with task ID and _allowed_ persons
genTask :: [Person] -> Int -> Gen Task
genTask ps tid = Task <$> return tid
                      <*> arbitrary
                      <*> arbitrary
                      <*> sublistOf ps

-- |  A QuickCheck 'Gen' variant that can count
--    (helpful for unique IDs).
type CountingGen = CountingT Gen

scaledListOf :: (Int -> Int) -> CountingGen a -> CountingGen [a]
scaledListOf s g = do
  oldSize <- lift $ getSize
  newSize <- lift $ chooseInt (0, s oldSize)
  replicateM newSize g

countingGenTaskTree :: [Person] -> CountingGen (Tree Task)
countingGenTaskTree ps = do
  tid       <- next
  task      <- lift $ genTask ps tid
  children  <- scaledListOf (`div` 50) $ countingGenTaskTree ps
  return $ Node task children

-- |  Task tree generator with unique IDs and _allowed_ persons
genTaskTree :: [Person] -> Gen (Tree Task)
genTaskTree ps = runCountingT (countingGenTaskTree ps) =<< arbitrary

-- |  Task forest generator with unique IDs and _allowed_ persons
genTaskForest :: [Person] -> Gen Tasks
genTaskForest ps = runCountingT gtf =<< arbitrary
  where gtf = scaledListOf (`div` 2) $ countingGenTaskTree ps

-- |  Generate a full data setting with non-empty persons
--    with unique person ids and tasks with valid assignees
genPersonsTasks :: Gen ([Person], Tasks)
genPersonsTasks = do
  ps <- genPersons `suchThat` (not.null)
  ts <- genTaskForest ps
  return (ps, ts)
