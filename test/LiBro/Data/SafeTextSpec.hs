module LiBro.Data.SafeTextSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Text.Arbitrary

import LiBro.Data.SafeText
import Data.String
import qualified Data.Text as T
import Control.Monad
import Control.Exception

spec :: Spec
spec = describe "SafeText wrapper" $
  modifyMaxDiscardRatio (const 42) $ do -- Neccessary, but tests are fast
    safetyChecks
    isStringInstance
    safePacking
    arbitraryInstance

safetyChecks :: Spec
safetyChecks = describe "Safety checks" $ do

  it "Correct unsafe characters" $
    unsafeChars `shouldBe` "\NUL\r"

  prop "Check safe strings" $
    \s -> safe s ==> isSafeString s

  prop "Check unsafe strings" $
    \s -> unsafe s ==> not (isSafeString s)

  prop "Check Text" $
    \t -> classify (safe (T.unpack t)) "safe" $
          classify (unsafe (T.unpack t)) "unsafe" $
          isSafeText t `shouldBe` isSafeString (T.unpack t)

  where safe    = all (`notElem` unsafeChars)
        unsafe  = any (`elem` unsafeChars)

isStringInstance :: Spec
isStringInstance = describe "IsString instance (OverloadedStrings)" $ do

  forM_ unsafeChars $ \char -> do
    let testStr = "foo" ++ [char] ++ "bar"
    it ("Throws an error with " ++ show char) $
      evaluate (fromString testStr :: SafeText)
        `shouldThrow` errorCall ("Not a safe string: " ++ show testStr)

  prop "No error without unsafe characters" $
    \s -> isSafeString s ==> getText (fromString s) `shouldBe` T.pack s

safePacking :: Spec
safePacking = describe "Safe packing" $ do

  prop "Pack safe Text" $
    \t -> isSafeText t ==> getText <$> safePackText t `shouldBe`  Just t

  prop "Pack unsafe Text" $
    \t -> not (isSafeText t) ==> safePackText t `shouldBe` Nothing

  prop "Check String input" $
    \s -> safePack s `shouldBe` safePackText (T.pack s)

arbitraryInstance :: Spec
arbitraryInstance = describe "Arbitrary instance (QuickCheck)" $ do

  prop "A long list of SafeTexts contains the answer" $
    forAll (arbitrary `suchThat` ((> 100) . length)) $ \sts -> do
      let everything = concat (map (T.unpack . getText) sts)
      forAll (elements "The answer is 42") (`elem` everything)
