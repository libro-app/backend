module LiBro.WebService.API where

import LiBro.WebService.Types
import Data.Proxy
import Servant.API

type LiBroAPI = "hello" :> Get '[JSON]      PersonIDs
          :<|>  "yay"   :> Get '[PlainText] String

libroApi :: Proxy LiBroAPI
libroApi = Proxy
