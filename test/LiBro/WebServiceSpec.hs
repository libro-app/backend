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
  personSpecs

personSpecs :: Spec
personSpecs = describe "Person related endpoints" $ with lws $ do

  describe "Person ID listing endpoint" $ do
    it "Respond with IDs" $ do
      get "/person" `shouldRespondWith`
        [json|{"personIDs": [1,2]}|]
        {matchStatus = 200}

  describe "Person details endpoint" $ do
    it "Respond with correct details" $ do
      get "/person/2" `shouldRespondWith`
        [json|{"pid": 2, "name": "Baz Quux", "email": "baz@quux.com"}|]
        {matchStatus = 200}

  where lws = libro <$> initLiBroState cfg
        cfg = Config (def {directory = "test/storage-files/data"}) def
