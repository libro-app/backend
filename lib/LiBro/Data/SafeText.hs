-- |  Thin wrapper around 'Text', but without unsafe characters.
module LiBro.Data.SafeText
  (
  -- * Definition of /unsafe/
  unsafeChars
  -- * A thin 'Text' wrapper with smart data constructor
  -- $smartDC
  , SafeText
  , getText
  -- * Safety checks
  , isSafeChar
  , isSafeText
  , isSafeString
  -- * Explicit value creation
  , safePackText
  , safePack
  , safeModify
  -- * Other useful stuff
  , safeTextParser
  ) where

import LiBro.Util
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Maybe
import Data.Aeson
import Data.Csv
import Test.QuickCheck

-- |  A list of all characters considered /unsafe/ in our setting:
--    @'\\NUL'@ and @'\\r'@.
unsafeChars :: [Char]
unsafeChars = "\NUL\r"

{- $smartDC
'SafeText' is only a thin @newtype@ wrapper around 'Text'. To ensure
that its characters are /safe/, the standard data constructor is hidden
from exports.

== How to create 'SafeText' values?

* Use `safePack` or 'safePackText' to create @'Just' 'SafeText'@ values
  (or 'Nothing' if the given text or string was /unsafe/).
* Use 'SafeText'\'s 'Read' instance together with 'read' or
  'Text.Read.readMaybe'.
* Use 'SafeText'\'s 'IsString' instance together with the
  [@OverloadedStrings@](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html)
  extension to create 'SafeText' directly from string literals:
  @"Hello, world" :: SafeText@

== Can 'SafeText' be used exactly like 'Text'?

No. There are very useful instances: 'Arbitrary' for property tests with
"Test.QuickCheck", 'ToJSON' and 'FromJSON' for JSON stuff with "Data.Aeson"
and 'ToField' and 'FromField' for CSV stuff with "Data.Csv" (Cassava).

Also, there's 'safeModify' that allows 'Text' modifying functions to be
applied /inside/ a 'SafeText'. But it seems to be a bit overkill to export
everything "Data.Text" has to offer.
-}

-- |  A simple @newtype@ wrapper around 'Text', but ensures the absence
--    of /unsafe/ characters.
newtype SafeText = SafeText
  { getText :: Text -- ^ Extracts the 'Text' value from 'SafeText'.
  } deriving Eq

instance Show SafeText where
  show = show . getText

-- |  A simple 'ReadS' parser for 'SafeText', useful for 'Read' instances.
safeTextParser :: ReadS SafeText
safeTextParser input
  | null unsafe = [(safeText, "")]
  | otherwise   = []
  where (safe, unsafe)  = span isSafeChar input
        (Just safeText) = safePack safe

instance Read SafeText where
  readsPrec _ = safeTextParser

-- |  Checks if a 'Char' is considered /safe/.
isSafeChar :: Char -> Bool
isSafeChar = (`notElem` unsafeChars)

-- |  Checks if a 'Text' is considered /safe/.
isSafeText :: Text -> Bool
isSafeText = T.all isSafeChar

-- |  Checks if a 'GHC.Base.String' is considered /safe/.
isSafeString :: String -> Bool
isSafeString = all isSafeChar

-- |  Creates a 'SafeText' value or 'Nothing'
--    if the given 'Text' was /unsafe/.
safePackText :: Text -> Maybe SafeText
safePackText = fmap SafeText . guarded isSafeText

-- |  Creates a 'SafeText' value or 'Nothing'
--    if the given 'GHC.Base.String' was /unsafe/.
safePack :: String -> Maybe SafeText
safePack = safePackText . T.pack

-- |  Safe application of a function that modifies 'Text' values.
safeModify :: (Text -> Text) -> SafeText -> Maybe SafeText
safeModify m = safePackText . m . getText

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
