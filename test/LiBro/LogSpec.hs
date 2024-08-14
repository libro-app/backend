module LiBro.LogSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import LiBro.Base
import LiBro.Log
import Data.Default
import Data.Time.Clock
import Data.Tuple.Extra

spec :: Spec
spec = describe "Logging" $ do
  format
  collection

format :: Spec
format = describe "Log format" $ do

  describe "Show instance" $ do
    now <- runIO getCurrentTime
    it "Correct log stringification" $
      show (Log now WARNING "foo" "bar")
        `shouldBe` "WARNING [" ++ show now ++ "] (foo): bar"

  describe "Within LiBroIO monad" $ do
    now <- runIO getCurrentTime
    (_, logs) <- runIO $ runLiBroIOLogs def $ addLog WARNING "foo" "bar"
    it "Exactly one log" $ length logs == 1
    let l = head logs
    it "Correct log level"    $ level   l `shouldBe` WARNING
    it "Correct log source"   $ source  l `shouldBe` "foo"
    it "Correct log message"  $ message l `shouldBe` "bar"
    it "Logged time is close enough" $
      (time l `diffUTCTime` now) `shouldSatisfy` (< 0.42)

genLogData :: Gen (LogLevel, LogSource, LogMessage)
genLogData = (,,) <$> chooseEnum (minBound, maxBound)
                  <*> arbitrary
                  <*> arbitrary

collection :: Spec
collection = describe "Collection of logs" $ do

  prop "Correct list of logs" $
    forAll (listOf genLogData) $ \lds -> ioProperty $ do
      (_, logs) <- runLiBroIOLogs def $ uncurry3 addLog `mapM_` lds
      let logTuples = map ((,,) <$> level <*> source <*> message) logs
      logTuples `shouldBe` lds
  
  prop "Ordered by time" $
    forAll (listOf genLogData) $ \lds -> ioProperty $ do
      (_, logs) <- runLiBroIOLogs def $ uncurry3 addLog `mapM_` lds
      time <$> logs `shouldSatisfy` isSorted
  
  where isSorted = and . (zipWith (<=) <*> tail)
