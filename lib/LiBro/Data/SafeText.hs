module LiBro.Data.SafeText
  ( unsafeChars
  , SafeText(getText)
  , isSafeText
  , isSafeString
  , safePackText
  , safePack
  ) where

import LiBro.Util
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Arbitrary
import Data.String
import Test.QuickCheck

unsafeChars :: String
unsafeChars = "\NUL\r"

newtype SafeText = SafeText { getText :: Text } deriving (Eq, Show)

instance IsString SafeText where
  fromString s
    | isSafeString s  = SafeText (T.pack s)
    | otherwise       = error ("Not a safe string: " ++ show s)

isSafeText :: Text -> Bool
isSafeText = isSafeString . T.unpack

isSafeString :: String -> Bool
isSafeString = all (`notElem` unsafeChars)

safePackText :: Text -> Maybe SafeText
safePackText = fmap SafeText . guarded isSafeText

safePack :: String -> Maybe SafeText
safePack = safePackText . T.pack

instance Arbitrary SafeText where
  arbitrary = suchThatMap arbitrary safePackText
