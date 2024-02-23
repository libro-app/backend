module LiBro.WebServiceSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import LiBro.Config
import LiBro.Control
import LiBro.WebService
import Data.Default

spec :: Spec
spec = describe "RESTful JSON web service" $ do
  listings

listings :: Spec
listings = describe "Simple data listing" $ with lws $ do

  context "Person listing endpoints" $ do

    describe "ID listing" $ do
      it "Correct IDs" $ do
        get "/person" `shouldRespondWith`
          [json|{"personIDs": [1,2]}|]
          {matchStatus = 200}

    describe "details" $ do
      it "Correct details" $ do
        get "/person/2" `shouldRespondWith`
          [json|{"pid": 2, "name": "Baz Quux", "email": "baz@quux.com"}|]
          {matchStatus = 200}

  context "Task listing endpoints" $ do

    describe "ID listing" $ do
      it "Correct IDs" $ do
        get "/task" `shouldRespondWith`
          [json|{"taskIDs": [17]}|]
          {matchStatus = 200}

  where lws = libro <$> initLiBroState cfg
        cfg = Config (def {directory = "test/storage-files/data"}) def
