module Main where

import Test.Hspec
import LiBro.Util

import qualified LiBro.DataSpec           as Data
import qualified LiBro.Data.SafeTextSpec  as Data.SafeText
import qualified LiBro.Data.StorageSpec   as Data.Storage
import qualified LiBro.ConfigSpec         as Config
import qualified LiBro.ControlSpec        as Control
import qualified LiBro.TestUtilSpec       as TestUtil
import qualified LiBro.UtilSpec           as Util
import qualified LiBro.WebServiceSpec     as WebService

withLibreOffice :: IO () -> IO ()
withLibreOffice runTests = do
  alreadyRunning <- libreOfficeIsRunning
  if alreadyRunning
    then do
      runTests
    else do
      spawnLibreOffice
      runTests
      killLibreOffice

main :: IO ()
main = hspec $ aroundAll_ withLibreOffice $ do
  Data.spec
  Data.SafeText.spec
  Data.Storage.spec
  Config.spec
  Control.spec
  TestUtil.spec
  Util.spec
  WebService.spec
