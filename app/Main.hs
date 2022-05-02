module Main where

import qualified OfcCalc (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  OfcCalc.someFunc
