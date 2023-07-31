module LiBro.Data.StorageSpec where

import Test.Hspec

import LiBro.Data
import LiBro.Data.Storage
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Either
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map
import Data.Tree
import Data.Csv
import Codec.Xlsx
import Control.Lens
import System.FilePath
import System.IO.Temp

spec :: Spec
spec = describe "Data storage" $ do
  taskCsv
  tasksToRecords
  recordsToTasks
  excelExport

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

excelExport :: Spec
excelExport = describe "Excel export" $ do

  context "Simple CSV -> XLSX conversion" $ do
    result <- runIO $ withSystemTempDirectory "excel-export" $ \tdir -> do
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
