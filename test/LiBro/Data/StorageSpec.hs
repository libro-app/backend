module LiBro.Data.StorageSpec where

import Test.Hspec

import LiBro.Data.Storage
import Data.Either
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Csv

spec :: Spec
spec = describe "Data storage" $ do
  taskCsv

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
