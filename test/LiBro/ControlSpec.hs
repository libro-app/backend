module LiBro.ControlSpec where

import Test.Hspec

import LiBro.Base
import LiBro.Config
import LiBro.Data
import LiBro.Data.Storage
import LiBro.Control
import Data.Default
import Data.Tree
import qualified Control.Concurrent as Conc
import System.IO.Temp

spec :: Spec
spec = describe "Control flow" $ do
  dataInitialization
  dataStorage

dataInitialization :: Spec
dataInitialization = describe "Blocking data loading" $ do

  context "With simple data files" $ do
    let config = def { storage = def { directory = "test/storage-files/data" }}
    expectedData  <- runIO $ runLiBroIO config loadData
    blocking      <- runIO $ Conc.newEmptyMVar
    libroData     <- runIO $ Conc.newEmptyMVar
    (beb, bed, aeb, aned, ld) <- runIO $ do
      beforeEmptyBlocking <- Conc.isEmptyMVar blocking
      beforeEmptyData     <- Conc.isEmptyMVar libroData
      runLiBroIO config $ initData blocking libroData
      afterEmptyBlocking  <- Conc.isEmptyMVar blocking
      afterNonEmptyData   <- Conc.isEmptyMVar libroData
      loadedData          <- Conc.readMVar libroData
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
    blocking  <- runIO $ Conc.newMVar Reading
    libroData <- runIO $ Conc.newMVar ldata
    rv <- runIO $ withSystemTempDirectory "storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      runLiBroIO config $ saveData blocking libroData
    it "Saving returns False" $ rv `shouldBe` False

  context "Manual saving of simple data" $ do
    blocking  <- runIO $ Conc.newEmptyMVar
    libroData <- runIO $ Conc.newMVar ldata
    testData  <- runIO $ withSystemTempDirectory "storage" $ \tdir -> do
      let config = def { storage = def { directory = tdir }}
      beforeEmptyBlocking <- Conc.isEmptyMVar blocking
      beforeLibroData     <- Conc.readMVar libroData
      returnValue         <- runLiBroIO config $ saveData blocking libroData
      afterEmptyBlocking  <- Conc.isEmptyMVar blocking
      afterLibroData      <- Conc.readMVar libroData
      storedData          <- runLiBroIO config loadData
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
