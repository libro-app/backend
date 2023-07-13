module AnswerSpec where

import Test.Hspec
import Test.QuickCheck

import Answer

spec :: Spec
spec = describe "Answer to life, universe and everything" $ do
  it "Answer to trivial question" $
    answer question `shouldBe` 42
  it "Answer is always the same" $
    property $ \q -> answer q `shouldBe` answer question
  where question = "Hello?"
