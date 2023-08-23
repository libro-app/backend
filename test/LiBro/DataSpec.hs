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
    let persons = [ Person 17 "foo" "bar", Person 42 "baz" "quux" ]
        tasks   = [ Task 1 "1t" "1d" [persons !! 0]
                  , Task 2 "2t" "2d" [persons !! 0, persons !! 1]
                  , Task 3 "3t" "3d" []
                  ]
        taskF     = [ Node (tasks !! 0) [Node (tasks !! 1) []]
                  , Node (tasks !! 2) []
                  ]
    it "Correct tasks for person 1" $
      tid <$> assignedTasks (persons !! 0) taskF `shouldBe` [1, 2]
    it "Correct tasks for person 2" $
      tid <$> assignedTasks (persons !! 1) taskF `shouldBe` [2]

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
    forAll (listOf genericArbitrary) $ \persons ->
    personMap persons `shouldBe` M.fromList (map ((,) =<< pid) persons)
