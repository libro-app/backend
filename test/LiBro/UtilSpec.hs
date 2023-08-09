module LiBro.UtilSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import LiBro.Util
import Data.Tree
import Data.Functor.Identity
import Control.Monad

spec :: Spec
spec = describe "Helper stuff" $ do
  forestFromParentList
  countingT
  
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
