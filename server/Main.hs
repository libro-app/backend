module Main where

import LiBro.Config as Conf
import LiBro.WebService
import Network.Wai.Handler.Warp

configuredMain :: Config -> IO ()
configuredMain cfg = do
  let port = Conf.port $ Conf.server cfg
  putStrLn $ "Serving LiBro backend on port " ++ show port ++ "."
  run port (libro cfg)

main :: IO ()
main = readConfig >>= maybe complain configuredMain
  where complain = putStrLn "Invalid config: aborting"
