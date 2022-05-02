module Main (main) where

import Test.HUnit(runTestTT, Counts, Test (TestList))
import qualified SuitTest as Suit
import qualified ValueTest as Value

main :: IO Counts
main = runTestTT $ TestList $ Suit.result ++ Value.result
