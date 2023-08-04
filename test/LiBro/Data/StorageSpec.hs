module LiBro.Data.StorageSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Text.Arbitrary
import Test.QuickCheck.Arbitrary.Generic

import LiBro.Config
import LiBro.Data
import LiBro.Data.Storage
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Maybe
import Data.Either
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map ((!))
import qualified Data.Map as M
import Data.Tree
import Data.Csv
import Codec.Xlsx
import Control.Lens
import System.FilePath
import System.IO.Temp
import Control.Monad

instance Arbitrary Person where
  arbitrary = genericArbitrary

instance Arbitrary IdList where
  arbitrary = genericArbitrary

spec :: Spec
spec = describe "Data storage" $ do
  personMapping
  idList
  taskCsv
  tasksToRecords
  recordsToTasks
  excelExport
  excelImport
  personStorage
  taskStorage

personMapping :: Spec
personMapping = describe "Map creation for Persons" $ do
  prop "Map from person IDs matches given persons" $ \persons ->
    personMap persons `shouldBe` M.fromList (map ((,) =<< pid) persons)

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

excelExport :: Spec
excelExport = describe "Excel export" $ do

  context "Simple CSV -> XLSX conversion" $ do
    result <- runIO $ withSystemTempDirectory "test-export" $ \tdir -> do
      let xlsxFile = tdir </> "data.xlsx"
      storeCSVasXLSX xlsxFile "foo,bar\r\n42,17\r\n"
      xlsxBS <- BS.readFile xlsxFile
      return $ toXlsx xlsxBS ^? ixSheet "export"
    it "Got a result" $
      result `shouldSatisfy` isJust
    let sheet   = fromJust result
        content = do
          h1 <- sheet ^? ixCell (1,1) . cellValue . _Just
          h2 <- sheet ^? ixCell (1,2) . cellValue . _Just
          v1 <- sheet ^? ixCell (2,1) . cellValue . _Just
          v2 <- sheet ^? ixCell (2,2) . cellValue . _Just
          return [(h1,h2), (v1,v2)]
    it "Got all table cells" $
      content `shouldSatisfy` isJust
    it "Correct table values" $
      fromJust content `shouldBe` [ (CellText "foo", CellText "bar")
                                  , (CellDouble 42, CellDouble 17)
                                  ]

  context "TaskRecords -> XLSX" $ do
    let taskRecord = TaskRecord 42 Nothing "foo" "bar" (IdList [17, 37])
    result <- runIO $ withSystemTempDirectory "test-export" $ \tdir -> do
      let xlsxFile = tdir </> "data.xlsx"
      storeCSVasXLSX xlsxFile (encodeDefaultOrderedByName [taskRecord])
      xlsxBS <- BS.readFile xlsxFile
      return $ toXlsx xlsxBS ^? ixSheet "export"
    it "Got a result" $
      result `shouldSatisfy` isJust
    let sheet = fromJust result
        parsedSheet = do
          (CellText h1) <- sheet ^? ixCell (1,1) . cellValue . _Just
          (CellText h2) <- sheet ^? ixCell (1,2) . cellValue . _Just
          (CellText h3) <- sheet ^? ixCell (1,3) . cellValue . _Just
          (CellText h4) <- sheet ^? ixCell (1,4) . cellValue . _Just
          (CellText h5) <- sheet ^? ixCell (1,5) . cellValue . _Just
          let columns = [h1, h2, h3, h4, h5]
          (CellDouble xTid) <- sheet ^? ixCell (2,1) . cellValue . _Just
          guard.isNothing $ sheet ^? ixCell (2,2) . cellValue . _Just
          (CellText xTitle) <- sheet ^? ixCell (2,3) . cellValue . _Just
          (CellText xDescr) <- sheet ^? ixCell (2,4) . cellValue . _Just
          (CellText xAss)   <- sheet ^? ixCell (2,5) . cellValue . _Just
          let record = TaskRecord (floor xTid) Nothing
                        xTitle xDescr (read $ T.unpack xAss)
          return (columns, [record])
    it "Sheet parsed correctly" $
      parsedSheet `shouldSatisfy` isJust
    let (Just (header, records)) = parsedSheet
    it "Correct header columns" $
      header `shouldBe` T.words "trid parentTid tTitle tDescription tAssignees"
    it "Correct task records" $
      records `shouldBe` [taskRecord]

excelImport :: Spec
excelImport = describe "Excel import" $ do

  context "With simple XLSX file" $ do
    csv <- runIO $ loadCSVfromXLSX "test/storage-files/simple.xlsx"
    let csvData = decode NoHeader csv :: Either String (Vector (String, String))
    it "Load correct CSV" $
      csvData `shouldBe` Right (V.fromList [("foo", "bar"), ("17", "42")])

  context "With simple TaskRecord data in XLSX file" $ do
    csv <- runIO $ loadCSVfromXLSX "test/storage-files/tasks.xlsx"
    let taskRecords = decode HasHeader csv
    it "Load correct TaskRecord" $
      taskRecords `shouldBe`
        Right (V.fromList [TaskRecord 42 Nothing "foo" "bar" (IdList [17, 37])])

personStorage :: Spec
personStorage = describe "XSLX storage of Person data" $ do
  modifyMaxSuccess (const 5) $
    prop "Load . store = id" $ \persons -> ioProperty $ do
      withSystemTempDirectory "person-storage" $ \tdir -> do
        let config = def { storage = def { directory = tdir }}
        storePersons config persons
        loadedPersons <- loadPersons config
        return $ loadedPersons === persons

taskStorage :: Spec
taskStorage = describe "XLSX storage of Task data" $ do
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

  describe "With empty data" $ do
    loadedTasks <- runIO $ withSystemTempDirectory "task-storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      storeTasks config []
      loadTasks config persons
    it "Got empty task forest" $
      loadedTasks `shouldBe` []

  describe "With some task data" $ do
    loadedTasks <- runIO $ withSystemTempDirectory "task-storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      storeTasks config tasks
      loadTasks config persons
    it "Got the right task forest" $
      loadedTasks `shouldBe` tasks
