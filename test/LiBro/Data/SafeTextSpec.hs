module LiBro.Data.SafeTextSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Text.Arbitrary () -- Instances only

import LiBro.Data.SafeText
import Data.String
import Data.Maybe
import Data.Either.Extra
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as J
import qualified Data.Csv as C
import Data.Csv (Only(..), HasHeader(..))
import Text.Read
import Control.Monad
import Control.Exception

spec :: Spec
spec = describe "SafeText wrapper" $
  modifyMaxDiscardRatio (const 1000) $ do -- Neccessary, but tests are fast
    safetyChecks
    safePacking
    textModification
    showInstance
    readInstance
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

textModification :: Spec
textModification = describe "Inner modifications" $ do
  
  prop "Identity does nothing" $ \st ->
    safeModify id st `shouldBe` Just st

  describe "Simple non-trivial example" $
    let answerTo42    = T.unwords . map a242 . T.words
        a242 "answer" = "42"
        a242 text     = text
    in  it "Replace answer word by 42" $
          safeModify answerTo42 "foo answer bar"
            `shouldBe` Just "foo 42 bar"

  describe "Replacing with something safe" $
    prop "Always retrieve the safe text" $ \(t, st) ->
      isSafeText t ==>
      getText <$> safeModify (const t) st `shouldBe` Just t

  describe "Replacing with something unsafe" $
    prop "Always get Nothing" $ \(t, st) ->
      not (isSafeText t) ==>
      safeModify (const t) st `shouldBe` Nothing

  describe "Appending something safe" $
    prop "Appending 'Just' appends" $ \(t, st) ->
      isSafeText t ==>
      getText <$> safeModify (`T.append` t) st
        `shouldBe` Just (getText st `T.append` t)

  describe "Appending something unsafe" $
    prop "Always get Nothing" $ \(t, st) ->
      not (isSafeText t) ==>
      safeModify (`T.append` t) st `shouldBe` Nothing

  context "Modification diagrams commute" $ do
    let modifications = [ (T.reverse, "reverse")
                        , (T.toTitle, "toTitle")
                        , (T.center 42 '%', "center to width 42 with '%'")
                        ]
    forM_ modifications $ \(m, name) ->
      prop ("Modification: " ++ name ++ ")") $ \st ->
        getText <$> safeModify m st
          `shouldBe` Just (m $ getText st)

showInstance :: Spec
showInstance = describe "Show instance" $ do
  prop "Same result as Text on safe input" $ \t ->
    isSafeText t ==>
    let st = fromJust $ safePackText t
    in  show st `shouldBe` show t

readInstance :: Spec
readInstance = describe "Read instance" $ do

  describe "Manual parsing with ReadS" $ do

    context "Simple cases" $ do
      it "Empty input -> empty output" $
        safeTextParser "" `shouldBe` [("","")]
      it "Safe input -> complete output" $
        safeTextParser "foo bar\nbaz" `shouldBe` [("foo bar\nbaz","")]
      it "Unsafe input -> no output" $
        safeTextParser "foo\rbar\nbaz" `shouldBe` []

    context "General case" $ do
      prop "Separate safe chars from unsafe chars" $ \s ->
        classify (isSafeString s) "safe" $
        classify (not $ isSafeString s) "unsafe" $
        safeTextParser s
          `shouldBe` maybe [] (\st -> [(st,"")]) (safePack s)

  describe "Getting Maybe results with readMaybe" $ do

    context "Simple cases" $ do
      it "Empty input -> empty output" $
        (readMaybe "" :: Maybe SafeText) `shouldBe` Just ""
      it "Safe input -> Just" $
        (readMaybe "foo bar\nbaz" :: Maybe SafeText)
          `shouldBe` Just "foo bar\nbaz"
      it "Unsafe input -> Nothing" $
        (readMaybe "foo\rbar\nbaz" :: Maybe SafeText) `shouldBe` Nothing

    context "General case" $ do
      prop "Only give safe Just results, otherwise Nothing" $ \s ->
        classify (isSafeString s) "safe" $
        classify (not $ isSafeString s) "unsafe" $
        readMaybe s `shouldBe` safePack s

  describe "Using read" $ do
    prop "Just create SafeText from safe strings" $ \s ->
      isSafeString s ==>
      (read s :: SafeText) `shouldBe` fromJust (safePack s)
    prop "Throw errors at unsafe strings" $ \s ->
      not (isSafeString s) ==>
      evaluate (read s :: SafeText) `shouldThrow` anyErrorCall

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
      let e = fromLeft "" $ readCSV (writeCSV' s)
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
