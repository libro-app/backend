module LiBro.Data.StorageSpec where

import Test.Hspec

import LiBro.Data
import LiBro.Data.Storage
import Data.Either
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map
import Data.Tree
import Data.Csv

spec :: Spec
spec = describe "Data storage" $ do
  taskCsv
  tasksToRecords
  recordsToTasks

taskCsv :: Spec
taskCsv = describe "Convert tasks <-> CSV" $ do

  context "With given task data" $ do
    it "Correct CSV output" $
      encodeDefaultOrderedByName tasks
        `shouldBe` csv

  context "With given invalid CSV data" $ do
    let result = decodeByName "LOL WUT"
          :: Either String (Header, Vector TaskRecord)
    it "Get error message" $ do
      result `shouldSatisfy` isLeft
      fromLeft "" result `shouldStartWith` "parse error"

  context "With given CSV data" $ do
    let result = decodeByName csv
    it "Correct tasks data" $
      result `shouldBe`
        Right (headerOrder (head tasks), V.fromList tasks)

    where tasks = [ TaskRecord 42 (Just 17) "foo" "bar" (IdList [])
                  , TaskRecord 17 Nothing "baz" "quux" (IdList [1, 67, 78926])
                  ]
          csv   = "trid,parentTid,tTitle,tDescription,tAssignees\r\n\
                  \42,17,foo,bar,\r\n\
                  \17,,baz,quux,1 67 78926\r\n\
                  \"

tasksToRecords :: Spec
tasksToRecords = describe "Task -> TaskRecord" $ do

  context "With given simple task forest" $ do
    let taskForest =
          [ Node (Task 17 "foo" "bar" []) []
          , Node (Task 42 "bar" "baz"
            [ Person 37 "Nina Schreubenmyrthe" "foo@bar"
            , Person 666 "Eugen Hammersbald" "baz@quux"
            ] )
            [ Node (Task 666 "baz" "quux" []) []
            , Node (Task 667 "quux" "quuux" []) []
            ]
          ]
        taskRecords =
          [ TaskRecord 17 Nothing "foo" "bar" (IdList [])
          , TaskRecord 42 Nothing "bar" "baz" (IdList [37, 666])
          , TaskRecord 666 (Just 42) "baz" "quux" (IdList [])
          , TaskRecord 667 (Just 42) "quux" "quuux" (IdList [])
          ]
    it "Correct task records" $
      storeTasks taskForest `shouldBe` taskRecords

recordsToTasks :: Spec
recordsToTasks = describe "TaskRecord -> Task" $ do

  context "With given simple track records and person table" $ do
    let persons = fromList
          [ (17, Person 17 "Nina Schreubenmyrthe" "foo@bar")
          , (42, Person 42 "Eugen Hammersbald" "baz@quux")
          ]
        taskRecords =
          [ TaskRecord 17 Nothing "foo" "bar" (IdList [])
          , TaskRecord 42 Nothing "bar" "baz" (IdList [37, 666])
          , TaskRecord 666 (Just 42) "baz" "quux" (IdList [])
          , TaskRecord 667 (Just 42) "quux" "quuux" (IdList [])
          ]
        taskForest =
          [ Node (Task 17 "foo" "bar" []) []
          , Node (Task 42 "bar" "baz"
            [ Person 37 "Nina Schreubenmyrthe" "foo@bar"
            , Person 666 "Eugen Hammersbald" "baz@quux"
            ] )
            [ Node (Task 666 "baz" "quux" []) []
            , Node (Task 667 "quux" "quuux" []) []
            ]
          ]
    it "Correct task forest" $
      loadTasks persons taskRecords `shouldBe` taskForest
