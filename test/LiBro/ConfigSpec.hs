module LiBro.ConfigSpec where

import Test.Hspec
import Test.QuickCheck

import LiBro.Config
import Data.Default
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T

-- This is maybe too easy, but it works OK
-- with Arbitrary Config below
writeConfig :: Config -> Text
writeConfig c = T.unlines
  [ "[storage]"
  , "directory = "      <> T.pack (directory s)
  , "tasks-file = "     <> T.pack (tasksFile s)
  , "tracking-file = "  <> T.pack (trackingFile s)
  ] <> "\n"
  where s = storage c

instance Arbitrary Config where
  arbitrary = do
    st <- Storage <$> name <*> name <*> name
    return $ Config st
    where chars = [choose ('a','z'), choose ('A','Z'), return '/']
          name  = do  a   <- oneof chars
                      z   <- oneof chars
                      as  <- listOf $ oneof (return ' ' : chars)
                      return (a : as ++ [z])

spec :: Spec
spec = describe "INI file configuration" $ do
  defaultConfig
  parsing

defaultConfig :: Spec
defaultConfig = describe "Default config values" $ do
  describe "Storage configuration" $ do
    let st = storage dc
    it "directory"      $ directory     st `shouldBe` "data-storage"
    it "tasks file"     $ tasksFile     st `shouldBe` "tasks.csv"
    it "tracking file"  $ trackingFile  st `shouldBe` "tracking.csv"
  where dc = def :: Config

parsing :: Spec
parsing = describe "Configuration parsing" $ do
  context "With simple values" $
    it "parse correct simple values" $ do
      let simple = Config $ Storage "foo" "bar" "baz"
      parseConfig (writeConfig simple) `shouldBe` Right simple
  context "With invalid ini input" $
    it "Parse result should be a Left" $
      parseConfig "42" `shouldSatisfy` isLeft
  context "With arbitrary configuration" $
    it "parseConfig . writeConfig = Right" $
      property $ \c -> parseConfig (writeConfig c) `shouldBe` Right c
