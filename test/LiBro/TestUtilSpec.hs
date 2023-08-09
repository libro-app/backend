module LiBro.TestUtilSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import LiBro.TestUtil
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = describe "Test utilities" $ do
  arbitraryText

arbitraryText :: Spec
arbitraryText = describe "Arbitrary instance for Data.Text" $ do

  prop "Non-empty Text generated" $
    -- Looks like a tautology, but demonstrates that
    -- the following property is not testing the empty set
    forAll (arbitrary `suchThat` ((> 42) . length)) $ \ts ->
      (ts :: [Text]) `shouldSatisfy` not . all T.null

  prop "Everything is printable" $  \t ->
    t `shouldSatisfy` T.all isPrint
