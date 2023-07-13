module Main where

import Answer

main :: IO ()
main = putStrLn "Your question?" >> getLine >>= print . answer
