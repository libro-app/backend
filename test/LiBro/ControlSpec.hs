module LiBro.ControlSpec where

import Test.Hspec

import LiBro.Base
import LiBro.Config
import LiBro.Data
import LiBro.Data.Storage
import LiBro.Control
import Data.Default
import Data.Tree
import Control.Concurrent
import System.IO.Temp

spec :: Spec
spec = describe "Control flow" $ do
  dataInitialization
  dataStorage

dataInitialization :: Spec
dataInitialization = describe "Blocking data loading" $ do

  context "With simple data files" $ do
    let config = def { storage = def { directory = "test/storage-files/data" }}
    expectedData  <- runIO $ runLiBro config loadData
    blocking      <- runIO $ newEmptyMVar
    libroData     <- runIO $ newEmptyMVar
    (beb, bed, aeb, aned, ld) <- runIO $ do
      beforeEmptyBlocking <- isEmptyMVar blocking
      beforeEmptyData     <- isEmptyMVar libroData
      runLiBro config $ initData blocking libroData
      afterEmptyBlocking  <- isEmptyMVar blocking
      afterNonEmptyData   <- isEmptyMVar libroData
      loadedData          <- readMVar libroData
      return
        ( beforeEmptyBlocking
        , beforeEmptyData
        , afterEmptyBlocking
        , afterNonEmptyData
        , loadedData
        )
    it "Blocking MVar is empty before"      $ beb `shouldBe` True
    it "LibroData MVar is empty before"     $ bed `shouldBe` True
    -- Can't check for blocking == Reading inbetween
    it "Blocking MVar is empty after"       $ aeb `shouldBe` True
    it "LibroData MVar is non-empty after"  $ aned `shouldBe` False
    it "Load correct data"                  $ ld `shouldBe` expectedData

dataStorage :: Spec
dataStorage = describe "Storing complete LiBro data" $ do

  -- Simple LibroData
  let ldPerson  = Person 17 "foo" "bar"
      ldTask    = Task 42 "baz" "quux" [ldPerson]
      ldata     = LBS (personMap [ldPerson]) [Node ldTask []]

  context "Manual saving while blocked" $ do
    blocking  <- runIO $ newMVar Reading
    libroData <- runIO $ newMVar ldata
    rv <- runIO $ withSystemTempDirectory "storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      runLiBro config $ saveData blocking libroData
    it "Saving returns False" $ rv `shouldBe` False

  context "Manual saving of simple data" $ do
    blocking  <- runIO $ newEmptyMVar
    libroData <- runIO $ newMVar ldata
    testData  <- runIO $ withSystemTempDirectory "storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      beforeEmptyBlocking <- isEmptyMVar blocking
      beforeLibroData     <- readMVar libroData
      returnValue         <- runLiBro config $ saveData blocking libroData
      afterEmptyBlocking  <- isEmptyMVar blocking
      afterLibroData      <- readMVar libroData
      storedData          <- runLiBro config loadData
      return
        ( beforeEmptyBlocking
        , beforeLibroData
        , returnValue
        , afterEmptyBlocking
        , afterLibroData
        , storedData
        )
    let (beb, bld, rv, aeb, ald, sd) = testData
    it "Blocking MVar is empty before"  $ beb `shouldBe` True
    it "There's LibroData before"       $ bld `shouldBe` ldata
    -- Can't check for blocking == Writing inbetween
    it "Correct return value"           $ rv `shouldBe` True
    it "Blocking MVar is empty after"   $ aeb `shouldBe` True
    it "LibroData unchanged after"      $ ald `shouldBe` bld
    it "Store correct data"             $ sd `shouldBe` ldata
