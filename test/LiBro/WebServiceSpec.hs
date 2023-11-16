module LiBro.WebServiceSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.QuickCheck

import LiBro.WebService.Server
import Data.ByteString

spec :: Spec
spec = describe "RESTful JSON web service" $ do
  helloLibro

helloLibro :: Spec
helloLibro = describe "Dummy: hello libro!" $ with (return libro) $ do

  describe "Hello endpoint" $ do
    it "Respond with 200 greeting" $ do
      get "/hello" `shouldRespondWith` "Hello LiBro!" {matchStatus = 200}

  describe "Any other endpoint" $ do
    it "Respond with 404" $ do
      property $ \endpoint ->
        show endpoint /= "hello" ==>
        get (pack endpoint) `shouldRespondWith` 404
