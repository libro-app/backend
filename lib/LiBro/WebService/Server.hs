module LiBro.WebService.Server where

import LiBro.WebService.API
import Servant

server :: Server LiBroAPI
server = return "Hello LiBro!"

libro :: Application
libro = serve libroApi server
