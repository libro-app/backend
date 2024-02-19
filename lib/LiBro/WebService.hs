module LiBro.WebService where

import LiBro.Config
import Data.Aeson
import Data.Proxy
import Servant
import GHC.Generics

newtype PersonIDs = PersonIDs {personIDs :: [Int]} deriving Generic
instance ToJSON PersonIDs

type LiBroAPI = "hello" :> Get '[JSON]      PersonIDs
          :<|>  "yay"   :> Get '[PlainText] String

libroServer :: Config -> Server LiBroAPI
libroServer cfg =     handleHello
                :<|>  handleYay
  where
        handleHello :: Handler PersonIDs
        handleHello = return $ PersonIDs [17, 42]

        handleYay :: Handler String
        handleYay = return "Yay!"

libroApi :: Proxy LiBroAPI
libroApi = Proxy

libro :: Config -> Application
libro = serve libroApi . libroServer
