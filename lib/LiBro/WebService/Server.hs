module LiBro.WebService.Server where

import LiBro.WebService.API
import LiBro.WebService.Types
import Servant

handleHello :: Handler PersonIDs
handleHello = return $ PersonIDs [17, 42]

handleYay :: Handler String
handleYay = return "Yay!"

libroServer :: Server LiBroAPI
libroServer =     handleHello
            :<|>  handleYay

libro :: Application
libro = serve libroApi libroServer
