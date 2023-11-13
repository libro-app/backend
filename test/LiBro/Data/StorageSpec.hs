module LiBro.Data.StorageSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import LiBro.TestUtil

import LiBro.Config
import LiBro.Data
import LiBro.Data.Storage
import Data.Either
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map ((!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import Data.Default
import System.IO.Temp

instance Arbitrary IdList where
  arbitrary = genericArbitrary

spec :: Spec
spec = describe "Data storage" $ do
  idList
  taskCsv
  tasksToRecords
  recordsToTasks
  personStorage
  taskStorage
  dataStorage

idList :: Spec
idList = describe "IdList String representation" $ do

  context "IdList -> String" $ do
    prop "Correct space-separated numbers" $
      \idl@(IdList is) -> idListToStr idl `shouldBe` unwords (show <$> is)
    prop "show behaves like idListToStr" $
      \idl -> show idl `shouldBe` idListToStr idl

  context "String -> IdList" $ do
    prop "Correct space-separated number parsing" $
      \is -> strToIdList (unwords (show <$> is)) `shouldBe` IdList is
    prop "read behaves like strToIdList" $
      \is ->  let isStr = unwords (show <$> (is :: [Int]))
              in  read isStr `shouldBe` strToIdList isStr

taskCsv :: Spec
taskCsv = describe "Convert TaskRecords <-> CSV" $ do

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
      tasksToTaskRecords taskForest `shouldBe` taskRecords

recordsToTasks :: Spec
recordsToTasks = describe "TaskRecord -> Task" $ do

  context "With given simple track records and person table" $ do
    let persons = personMap
          [ Person 17 "Nina Schreubenmyrthe" "foo@bar"
          , Person 42 "Eugen Hammersbald" "baz@quux"
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
      taskRecordsToTasks persons taskRecords `shouldBe` taskForest

personStorage :: Spec
personStorage = describe "XLSX storage of Person data" $ do

  describe "Loading without a file" $ do
    result <- runIO $ withSystemTempDirectory "person-storage" $ \tdir -> do
      loadPersons $ def { storage = def { directory = tdir }}
    it "Empty Person map" $
      result `shouldBe` M.empty

  modifyMaxSuccess (const 5) $
    prop "Load . store = id" $
      forAll genPersons $ \persons -> ioProperty $ do
        withSystemTempDirectory "person-storage" $ \tdir -> do
          let config = def { storage = def { directory = tdir }}
          storePersons config persons
          loadedPersons <- loadPersons config
          return $ loadedPersons === persons

taskStorage :: Spec
taskStorage = describe "XLSX storage of Task data" $ do

  describe "Loading without a file" $ do
    result <- runIO $ withSystemTempDirectory "task-storage" $ \tdir -> do
      loadTasks (def { storage = def { directory = tdir }}) M.empty
    it "Empty task list" $
      result `shouldBe` []

  let persons = personMap
        [ Person 17 "Nina Schreubenmyrthe" "foo@bar"
        , Person 42 "Eugen Hammersbald" "baz@quux"
        ]
      tasks   =
        [ Node (Task 37 "fooTitle" "fooDescr" []) []
        , Node (Task 67 "barTitle" "barDescr" [persons ! 17])
          [ Node (Task 87 "bazTitle" "bazDescr" [persons ! 42]) []
          , Node (Task 97 "quuxTitle" "quuxDescr" [persons ! 17, persons ! 42]) []
          ]
        ]

  describe "Storing empty data" $ do
    loadedTasks <- runIO $ withSystemTempDirectory "task-storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      storeTasks config []
      loadTasks config persons
    it "Got empty task forest" $
      loadedTasks `shouldBe` []

  describe "Storing some task data" $ do
    loadedTasks <- runIO $ withSystemTempDirectory "task-storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      storeTasks config tasks
      loadTasks config persons
    it "Got the right task forest" $
      loadedTasks `shouldBe` tasks

dataStorage :: Spec
dataStorage = describe "Complete dataset" $ do

  context "With simple data files" $ do
    let persons = [ Person 1 "Foo Bar" "foo@bar.com"
                  , Person 2 "Baz Quux" "baz@quux.com"
                  ]
        tasks   = [ Task 17 "t17" "d17" [persons !! 0, persons !! 1]
                  , Task 37 "t37" "d37" [persons !! 1]
                  , Task 42 "t42" "d42" []
                  ]
        tForest = [ Node (tasks !! 0)
                    [ Node (tasks !! 1) []
                    , Node (tasks !! 2) []
                    ]
                  ]
    let conf = def { storage = def { directory = "test/storage-files/data" }}
    (LBS loadedPersons loadedTasks) <- runIO $ loadData conf
    it "Load correct persons" $
      loadedPersons `shouldBe` personMap persons
    it "Load correct task forest" $
      loadedTasks `shouldBe` tForest

  context "With arbitrary datasets" $ do
    modifyMaxSuccess (const 5) $
      prop "load . store = id" $
        forAll genPersonsTasks $ \d ->
          ioProperty $ do
            withSystemTempDirectory "storage" $ \tdir -> do
              let conf = def { storage = def { directory = tdir }}
              storeData conf d
              loadedData <- loadData conf
              return $ loadedData `shouldBe` d
