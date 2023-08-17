module LiBro.Data.SafeText
  ( unsafeChars
  , SafeText
  , getText
  , isSafeText
  , isSafeString
  , safePackText
  , safePack
  ) where

import LiBro.Util
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Aeson
import Data.Csv
import Test.QuickCheck

unsafeChars :: String
unsafeChars = "\NUL\r"

newtype SafeText = SafeText { getText :: Text } deriving (Eq, Show)

isSafeText :: Text -> Bool
isSafeText = isSafeString . T.unpack

isSafeString :: String -> Bool
isSafeString = all (`notElem` unsafeChars)

safePackText :: Text -> Maybe SafeText
safePackText = fmap SafeText . guarded isSafeText

safePack :: String -> Maybe SafeText
safePack = safePackText . T.pack

instance IsString SafeText where
  fromString s
    | isSafeString s  = SafeText (T.pack s)
    | otherwise       = error ("Not a safe string: " ++ show s)

instance Arbitrary SafeText where
  arbitrary = suchThatMap arbitrary safePack

instance ToJSON SafeText where
  toJSON = toJSON . getText

instance FromJSON SafeText where
  parseJSON = withText "SafeText" $ \text ->
    case safePackText text of
      Just st -> return st
      Nothing -> fail $ "Unsafe string: " ++ show text

instance ToField SafeText where
  toField = toField . getText

instance FromField SafeText where
  parseField fbs = do
    text <- parseField fbs
    case safePackText text of
      Just st -> return st
      Nothing -> fail $ "Unsafe string: " ++ show text
