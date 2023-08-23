module LiBro.ControlSpec where

import Test.Hspec

import LiBro.Config
import LiBro.Data.Storage
import LiBro.Control
import Data.Default
import Control.Concurrent

spec :: Spec
spec = describe "Control flow" $ do
  dataInitialization

dataInitialization :: Spec
dataInitialization = describe "Blocking data loading" $ do

  context "With simple data files" $ do
    let config = def { storage = def { directory = "test/storage-files/data" }}
    expectedData  <- runIO $ loadData config
    blocking      <- runIO $ newEmptyMVar
    libroData     <- runIO $ newEmptyMVar
    (beb, bed, aeb, aned, ld) <- runIO $ do
      beforeEmptyBlocking <- isEmptyMVar blocking
      beforeEmptyData     <- isEmptyMVar libroData
      initData config blocking libroData
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
