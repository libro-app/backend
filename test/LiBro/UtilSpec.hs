module LiBro.UtilSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import LiBro.Util
import Data.Tree
import Data.Functor.Identity
import Control.Monad

spec :: Spec
spec = describe "Helper stuff" $ do
  forestFromParentList
  countingT
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
