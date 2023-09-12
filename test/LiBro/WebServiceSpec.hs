module LiBro.WebServiceSpec where

import Test.Hspec
import Test.Hspec.Wai

import LiBro.WebService.Server

spec :: Spec
spec = describe "RESTful JSON web service" $ do
  helloLibro

helloLibro :: Spec
helloLibro = describe "Dummy: hello libro!" $ with (return libro) $ do

  describe "Root" $ do
    it "Respond with 404" $ do
      get "/" `shouldRespondWith` 404

  describe "Hello endpoint" $ do
    it "Respond with greeting" $ do
      get "/hello" `shouldRespondWith` "Hello LiBro!"
