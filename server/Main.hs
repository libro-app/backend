module Main where

import LiBro.Config as Conf
import LiBro.WebService
import Network.Wai.Handler.Warp

configuredMain :: Config -> IO ()
configuredMain config = do
  let port = Conf.port $ Conf.server config
  putStrLn $ "Serving LiBro backend on port " ++ show port ++ "."
  run port libro

main :: IO ()
main = readConfig >>= maybe complain configuredMain
  where complain = putStrLn "Invalid config: aborting"
