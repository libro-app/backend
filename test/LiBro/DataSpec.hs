module LiBro.DataSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.Hspec.QuickCheck
import LiBro.TestUtil

import LiBro.Data
import qualified Data.Map as M
import Data.Tree

spec :: Spec
spec = describe "Data handling" $ do
  assigneesOfTasks
  personMapping

assigneesOfTasks :: Spec
assigneesOfTasks = describe "Assignees of tasks" $ do

  context "No tasks" $
    prop "No assigned tasks found" $
      forAll (arbitrary >>= genPerson) $ \person ->
          assignedTasks person [] `shouldBe` []

  context "Simple situation" $ do
    let myPersons = [ Person 17 "foo" "bar", Person 42 "baz" "quux" ]
        myTasks   = [ Task 1 "1t" "1d" [myPersons !! 0]
                    , Task 2 "2t" "2d" [myPersons !! 0, myPersons !! 1]
                    , Task 3 "3t" "3d" []
                    ]
        taskF     = [ Node (myTasks !! 0) [Node (myTasks !! 1) []]
                    , Node (myTasks !! 2) []
                    ]
    it "Correct tasks for person 1" $
      tid <$> assignedTasks (myPersons !! 0) taskF `shouldBe` [1, 2]
    it "Correct tasks for person 2" $
      tid <$> assignedTasks (myPersons !! 1) taskF `shouldBe` [2]

  context "Arbitrary tasks and persons" $ do

    prop "Assigned tasks' assignees contain that person" $
      forAll genPersonsTasks $ \(LBS ps ts) ->
        forAll (elements $ M.elems ps) $ \p ->
          (`all` assignedTasks p ts) $ \t ->  -- can't use forAll here as
            p `elem` assignees t              -- asigned tasks could be empty

    prop "# Assigned + # unassigned = # all tasks" $
      forAll genPersonsTasks $ \(LBS ps ts) ->
        forAll (elements $ M.elems ps) $ \p ->
          let pAssigned   = assignedTasks p ts
              flatTasks   = concatMap flatten ts
              unAssigned  = filter (not . (p `elem`) . assignees) flatTasks
          in  length pAssigned + length unAssigned === length flatTasks

personMapping :: Spec
personMapping = describe "Map creation for Persons" $ do
  prop "Map from person IDs matches given persons" $
    forAll (listOf genericArbitrary) $ \myPersons ->
    personMap myPersons `shouldBe` M.fromList (map ((,) =<< pid) myPersons)
