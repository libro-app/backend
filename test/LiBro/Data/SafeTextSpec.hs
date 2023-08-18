module LiBro.Data.SafeTextSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Text.Arbitrary

import LiBro.Data.SafeText
import Data.String
import Data.Either
import Data.Either.Extra
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as J
import qualified Data.Csv as C
import Data.Csv (Only(..), HasHeader(..))
import Control.Monad
import Control.Exception

spec :: Spec
spec = describe "SafeText wrapper" $
  modifyMaxDiscardRatio (const 1000) $ do -- Neccessary, but tests are fast
    safetyChecks
    safePacking
    showInstance
    isStringInstance
    arbitraryInstance
    jsonInstances
    csvInstances

safetyChecks :: Spec
safetyChecks = describe "Safety checks" $ do

  it "Correct unsafe characters" $
    unsafeChars `shouldBe` "\NUL\r"

  prop "Check safe characters" $ \c ->
    safe [c] ==> isSafeChar c

  prop "Check unsafe characters" $ \c ->
    unsafe [c] ==> not (isSafeChar c)

  prop "Check safe strings" $ \s ->
    safe s ==> isSafeString s

  prop "Check unsafe strings" $ \s ->
    unsafe s ==> not (isSafeString s)

  prop "Check Text" $ \t ->
    classify (safe (T.unpack t)) "safe" $
    classify (unsafe (T.unpack t)) "unsafe" $
    isSafeText t `shouldBe` isSafeString (T.unpack t)

  where safe    = all (`notElem` unsafeChars)
        unsafe  = any (`elem` unsafeChars)

safePacking :: Spec
safePacking = describe "Safe packing" $ do

  prop "Pack safe Text" $ \t ->
    isSafeText t ==> getText <$> safePackText t `shouldBe`  Just t

  prop "Pack unsafe Text" $ \t ->
    not (isSafeText t) ==> safePackText t `shouldBe` Nothing

  prop "Check String input" $ \s ->
    safePack s `shouldBe` safePackText (T.pack s)

showInstance :: Spec
showInstance = describe "Show instance" $ do
  prop "Same result as Text on safe input" $ \t ->
    isSafeText t ==>
    let (Just st) = safePackText t
    in  show st `shouldBe` show t

isStringInstance :: Spec
isStringInstance = describe "IsString instance (OverloadedStrings)" $ do

  forM_ unsafeChars $ \char -> do
    let testStr = "foo" ++ [char] ++ "bar"
    it ("Throws an error with " ++ show char) $
      evaluate (fromString testStr :: SafeText)
        `shouldThrow` errorCall ("Not a safe string: " ++ show testStr)

  prop "No error without unsafe characters" $ \s ->
    isSafeString s ==> getText (fromString s) `shouldBe` T.pack s

arbitraryInstance :: Spec
arbitraryInstance = describe "Arbitrary instance (QuickCheck)" $ do

  prop "A long list of SafeTexts contains the answer" $
    forAll (arbitrary `suchThat` ((> 100) . length)) $ \sts -> do
      let everything = concat (map (T.unpack . getText) sts)
      forAll (elements "The answer is 42") (`elem` everything)

jsonInstances :: Spec
jsonInstances = describe "JSON instances" $ do

  describe "Roundtrip FromJSON . ToJSON = id" $ do
    prop "Correct SafeText" $ \st ->
      J.decode (J.encode (st :: SafeText)) `shouldBe` Just st

  describe "SafeText accepts valid string input only" $ do
    prop "Parsing gives Nothing" $ \s ->
      not (isSafeString s) ==>
      (J.decode (J.encode s) :: Maybe SafeText) `shouldBe` Nothing

  describe "JSON roundtrip is equivalent to calling safePack" $
    prop "They're the same picture" $ \s ->
      classify (isSafeString s) "safe" $
      classify (not $ isSafeString s) "unsafe" $
      J.decode (J.encode s) `shouldBe` safePack s

csvInstances :: Spec
csvInstances = describe "CSV instances" $ do

  describe "Roundtrip FromField . ToField = id" $ do
    prop "Correct (non-empty) SafeText" $ \st ->
      not (T.null $ getText st) ==>
      readCSV (writeCSV st) `shouldBe` Right (V.fromList [Only st])

  describe "SafeText accepts valid string input only" $ do
    prop "Parsing gives error message" $ \s ->
      not (isSafeString s) ==>
      let (Left e) = readCSV (writeCSV' s)
          expected = "Unsafe string: " ++ show s
      in  e `shouldContain` expected

  describe "CSV roundtrip is somehow equivalent to calling safePack" $
    prop "They're the same picture" $ \s ->
      not (null s) ==>
      classify (isSafeString s) "safe" $
      classify (not $ isSafeString s) "unsafe" $
      eitherToMaybe (readCSV (writeCSV' s))
        `shouldBe` (V.fromList . return . Only <$> safePack s)

  where
    -- explicit types to make tests more readable
    writeCSV :: SafeText -> ByteString
    writeCSV = C.encode . return . Only
    writeCSV' :: String -> ByteString
    writeCSV' = C.encode . return . Only
    readCSV :: ByteString -> Either String (Vector (Only SafeText))
    readCSV = C.decode NoHeader
