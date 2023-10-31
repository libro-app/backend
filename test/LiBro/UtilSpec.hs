module LiBro.UtilSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import LiBro.Util
import LiBro.Data.SafeText
import Data.Tree
import Data.Functor.Identity
import Data.Csv
import Control.Monad
import GHC.Generics
import System.FilePath
import System.IO.Temp

spec :: Spec
spec = describe "Helper stuff" $ do
  forestFromParentList
  countingT
  xlsx
  guarding

forestFromParentList :: Spec
forestFromParentList = describe "Read Forest from parent list" $ do

  context "With simple parent list encoded forest" $ do
    let parentList =  [ (168, Just 84), (17, Nothing), (84, Just 42)
                      , (51, Just 17), (34, Just 17), (42, Nothing)
                      ]
    it "Read correct forest with sorted children" $
      readForest parentList `shouldBe`
        [ Node 17 [ Node 34 [], Node 51 []]
        , Node 42 [ Node 84 [ Node 168 [] ]]
        ]

countingT :: Spec
countingT = describe "The CountingT 'monad transformer'" $ do
  let nextTimes n = replicateM n next
  describe "Counting from arbitrary Ints" $ do
    prop "Grab the following Ints" $ \(start, len) -> do
      let results = runCountingT (nextTimes len) start
      runIdentity results `shouldBe` take len [start..]

data ExampleData = ED {foo :: Int, bar :: SafeText}
  deriving (Eq, Show, Generic)
instance FromNamedRecord ExampleData
instance ToNamedRecord ExampleData
instance DefaultOrdered ExampleData
instance Arbitrary ExampleData where arbitrary = genericArbitrary

xlsx :: Spec
xlsx = describe "XLSX data conversion" $ do

  describe "Reading from simple spreadsheet file" $ do
    result <- runIO $ loadFromXlsx "test/storage-files/simple.xlsx"
    it "Correct result" $
      result `shouldBe` Right [ED 17 "42"]

  describe "Load . store = id" $ do

    context "With simple example data" $ do
      let input = [ED 42 "Hallö\nWelt!", ED 0 "=foo"]
      output <- runIO $ withSystemTempDirectory "xlsx-export" $ \tdir -> do
        let fp = tdir </> "simple.xlsx"
        storeAsXlsx fp input
        loadFromXlsx fp
      it "Correct output data" $ output `shouldBe` Right input

    context "With arbitrary data" $ do
      modifyMaxSuccess (const 5) $
        prop "Correct output data" $ \d ->
          ioProperty $ withSystemTempDirectory "xlsx-export" $ \tdir -> do
            let fp = tdir </> "arbitrary.xlsx"
            storeAsXlsx fp d
            result <- loadFromXlsx fp
            return $ result `shouldBe` Right (d :: [ExampleData])

guarding :: Spec
guarding = describe "Guarded Alternative value creation" $ do

  context "Alternative: Maybe" $ do
    it "Guard a non-42 for 42-ness" $
      guarded (== 42) 17 `shouldBe` Nothing
    it "Guard a 42 for 42-ness" $
      guarded (== 42) 42 `shouldBe` Just 42
    prop "Guard even: Even" $ \i ->
      even (i :: Int) ==>
      guarded even i `shouldBe` Just i
    prop "Guard even: Odd" $ \i ->
      odd (i :: Int) ==>
      guarded even i `shouldBe` Nothing

  context "Alternative: List" $ do
    prop "Guard even: Even" $ \i ->
      even (i :: Int) ==>
      guarded even i `shouldBe` [i]
    prop "Guard even: Odd" $ \i ->
      odd (i :: Int) ==>
      guarded even i `shouldBe` []
