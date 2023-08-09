module LiBro.TestUtilSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import LiBro.TestUtil
import LiBro.Data
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Tree

spec :: Spec
spec = describe "Test utilities" $ do
  arbitraryText
  dataGeneration

arbitraryText :: Spec
arbitraryText = describe "Arbitrary instance for Data.Text" $ do

  prop "Non-empty Text generated" $
    -- Looks like a tautology, but demonstrates that
    -- the following property is not testing the empty set
    forAll (arbitrary `suchThat` ((> 42) . length)) $ \ts ->
      (ts :: [Text]) `shouldSatisfy` not . all T.null

  prop "Everything is printable" $  \t ->
    t `shouldSatisfy` T.all isPrint

dataGeneration :: Spec
dataGeneration = describe "Generation of arbitrary libro data" $ do

  describe "Person generation" $ do
    prop "Person has the right ID" $ \i ->
      forAll (genPerson i) $ \p ->
        pid p `shouldBe` i
    prop "Person list has unique IDs" $ do
      forAll genPersons $ \ps -> do
        let pids = pid <$> ps
        length (nub pids) `shouldBe` length pids

  describe "Task generation" $ do
    prop "Task has the right ID" $ \i ->
      forAll genPersons $ \ps ->
        forAll (genTask ps i) $ \t ->
          tid t `shouldBe` i
    prop "Task tree has unique IDs" $ do
      forAll genPersons $ \ps ->
        forAll (genTaskTree ps) $ \tt -> do
          let tids = tid <$> flatten tt
          length (nub tids) `shouldBe` length tids
    prop "Task tree has only allowed assignees" $ do
      forAll genPersons $ \ps ->
        forAll (genTaskTree ps) $ \tt ->
          (`all` concatMap assignees (flatten tt)) $ \p ->
            p `elem` ps
    prop "Task forest has unique IDs" $ do
      forAll genPersons $ \ps ->
        forAll (genTaskForest ps) $ \tf -> do
          let tids = tid <$> concatMap flatten tf
          length (nub tids) `shouldBe` length tids
    prop "Task forest has only allowed assignees" $ do
      forAll genPersons $ \ps ->
        forAll (genTaskForest ps) $ \tf ->
          let tasks = concatMap flatten tf
          in  (`all` concatMap assignees tasks) $ \as ->
                as `elem` ps
