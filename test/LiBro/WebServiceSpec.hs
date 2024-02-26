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

    describe "Person listing" $ do
      it "Correct list" $ do
        get "/person" `shouldRespondWith`
          [json|[
            {"pid": 1,  "name": "Foo Bar",  "email": "foo@bar.com"},
            {"pid": 2,  "name": "Baz Quux", "email": "baz@quux.com"}
          ]|]
          {matchStatus = 200}

    describe "details" $ do
      it "Correct details" $ do
        get "/person/2" `shouldRespondWith`
          [json|{
            "person": {"pid": 2, "name": "Baz Quux", "email": "baz@quux.com"},
            "personTasks": [
              {"tid": 17, "title": "t17", "description": "d17", "assignees": [
                {"pid": 1,  "name": "Foo Bar",  "email": "foo@bar.com"},
                {"pid": 2,  "name": "Baz Quux", "email": "baz@quux.com"}
              ]},
              {"tid": 37, "title": "t37", "description": "d37", "assignees": [
                {"pid": 2,  "name": "Baz Quux", "email": "baz@quux.com"}
              ]}
            ]
          }|]
          {matchStatus = 200}

      it "404 if person does not exist" $ do
        get "/person/42" `shouldRespondWith`
          "Person not found"
          {matchStatus = 404}

  context "Task listing endpoints" $ do

    describe "Top level tasks" $ do
      it "Correct list" $ do
        get "/task" `shouldRespondWith`
          [json|[
            {"tid": 17, "title": "t17", "description": "d17", "assignees": [
              {"pid": 1,  "name": "Foo Bar",  "email": "foo@bar.com"},
              {"pid": 2,  "name": "Baz Quux", "email": "baz@quux.com"}
            ]}
          ]|]
          {matchStatus = 200}

    describe "Full task hierarchy" $ do
      it "Correct forest" $ do
        get "/task/tree" `shouldRespondWith`
          [json|[
            { "task": {"tid": 17, "title": "t17", "description": "d17", "assignees": [
                {"pid": 1,  "name": "Foo Bar",  "email": "foo@bar.com"},
                {"pid": 2,  "name": "Baz Quux", "email": "baz@quux.com"}
              ]},
              "subTasks": [
                { "task": {"tid": 37, "title": "t37", "description": "d37", "assignees": [
                    {"pid": 2,  "name": "Baz Quux", "email": "baz@quux.com"}
                  ]},
                  "subTasks": []
                },
                { "task": {"tid": 42, "title": "t42", "description": "d42", "assignees": []},
                  "subTasks": []
                }
              ]
            }
          ]|]
          {matchStatus = 200}

  where lws = libro <$> initLiBroState cfg
        cfg = Config (def {directory = "test/storage-files/data"}) def
