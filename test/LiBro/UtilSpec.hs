module LiBro.UtilSpec where

import Test.Hspec

import LiBro.Util
import Data.Tree

spec :: Spec
spec = describe "Helper stuff" $ do
  forestFromParentList
  
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
