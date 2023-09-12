module LiBro.WebService.API where

import Data.Proxy
import Servant.API

type LiBroAPI = "hello" :> Get '[PlainText] String

libroApi :: Proxy LiBroAPI
libroApi = Proxy
