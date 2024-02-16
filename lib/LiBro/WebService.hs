module LiBro.WebService where

import Data.Aeson
import Data.Proxy
import Servant
import GHC.Generics

newtype PersonIDs = PersonIDs {personIDs :: [Int]} deriving Generic
instance ToJSON PersonIDs

type LiBroAPI = "hello" :> Get '[JSON]      PersonIDs
          :<|>  "yay"   :> Get '[PlainText] String

libroServer :: Server LiBroAPI
libroServer =     handleHello
            :<|>  handleYay
  where
        handleHello :: Handler PersonIDs
        handleHello = return $ PersonIDs [17, 42]

        handleYay :: Handler String
        handleYay = return "Yay!"

libroApi :: Proxy LiBroAPI
libroApi = Proxy

libro :: Application
libro = serve libroApi libroServer

