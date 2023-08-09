module LiBro.TestUtil where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

instance Arbitrary Text where
  arbitrary = T.pack . getPrintableString <$> arbitrary
