{-# OPTIONS_GHC -Wno-orphans #-}

module LiBro.ConfigSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import LiBro.Config
import Data.Default
import Data.Maybe
import Data.Either
import Data.Text as T
import System.IO
import System.IO.Temp
import System.IO.Silently

-- This is maybe too easy, but it works OK
-- with Arbitrary Config below
writeConfig :: Config -> Text
writeConfig c = T.unlines
  [ "[storage]"
  , "directory = "      <> T.pack (directory    st)
  , "person-file = "    <> T.pack (personFile   st)
  , "tasks-file = "     <> T.pack (tasksFile    st)
  , "tracking-file = "  <> T.pack (trackingFile st)
  , ""
  , "[server]"
  , "port = "           <> T.pack (show $ port srv)
  ] <> "\n"
  where st  = storage c
        srv = server c

instance Arbitrary Config where
  arbitrary = do
    st  <- Storage  <$> aname <*> aname <*> aname <*> aname
    srv <- Server   <$> aport
    return $ Config st srv
    where chars = '/' : ['a'..'z'] ++ ['A'..'Z']
          aname = do  a   <- elements chars
                      z   <- elements chars
                      as  <- listOf $ elements (' ' : chars)
                      return (a : as ++ [z])
          aport = elements [1024 .. 49151] -- Wikipedia "Registered port"

spec :: Spec
spec = describe "INI file configuration" $ do
  defaultConfig
  parsing
  reading

defaultConfig :: Spec
defaultConfig = describe "Default config values" $ do
  describe "Storage configuration" $ do
    let st = storage dc
    it "directory"      $ directory     st `shouldBe` "data-storage"
    it "person file"    $ personFile    st `shouldBe` "persons.xlsx"
    it "tasks file"     $ tasksFile     st `shouldBe` "tasks.xlsx"
    it "tracking file"  $ trackingFile  st `shouldBe` "tracking.xlsx"
  describe "Server configuration" $ do
    let srv = server dc
    it "port" $ port srv `shouldBe` 8080
  where dc = def :: Config

parsing :: Spec
parsing = describe "Configuration parsing" $ do

  context "With simple values" $
    it "parse correct simple values" $ do
      let simple = Config
                    (Storage "foo" "bar" "baz" "quux")
                    (Server 1742)
      parseConfig (writeConfig simple) `shouldBe` Right simple

  context "With invalid ini input" $
    it "parse result should be a Left" $
      parseConfig "42" `shouldSatisfy` isLeft

  context "With arbitrary configuration" $
    prop "parseConfig . writeConfig = Right" $ \c ->
      parseConfig (writeConfig c) `shouldBe` Right c

reading :: Spec
reading = describe "Reading configuration from file" $ do

  context "With existing test config file" $ do
    let simple = Config
                  (Storage "bar" "baz" "quux" "quuux")
                  (Server 4217)
    config <- runIO $ withSystemTempFile "config.ini" $ \fp h -> do
      hPutStr h (T.unpack $ writeConfig simple) >> hClose h
      readConfigFrom fp
    it "extract correct configuration" $
      config `shouldBe` Just simple

  context "With invalid file contents" $ do
    (errOutput, result) <- runIO $ withSystemTempFile "config.ini" $ \fp h -> do
      hPutStr h "ILLEGAL INI" >> hClose h
      hCapture [stderr] $ readConfigFrom fp
    it "no result" $
      result `shouldSatisfy` isNothing
    it "get parser error message" $ do
      errOutput `shouldStartWith` "Error parsing"
      errOutput `shouldContain` "ILLEGAL INI"
