module Main (main) where

import Test.HUnit(runTestTT, Counts, Test (TestList))
import qualified SuitTest
import qualified ValueTest

main :: IO Counts
main = runTestTT $ TestList $ SuitTest.result ++ ValueTest.result
