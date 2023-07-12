module Main where

import LiBro

main :: IO ()
main = putStrLn "Your question?" >> getLine >>= print . answer
