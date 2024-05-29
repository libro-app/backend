module Main where

import LiBro.Config
import LiBro.Control
import LiBro.WebService
import Network.Wai.Handler.Warp

configuredMain :: Config -> IO ()
configuredMain cfg = do
  let p = port $ server cfg
  putStrLn $ "Serving LiBro backend on port " ++ show p ++ "."
  initState <- initLiBroState cfg
  run p $ libro initState

main :: IO ()
main = readConfig >>= maybe complain configuredMain
  where complain = putStrLn "Invalid config: aborting"
