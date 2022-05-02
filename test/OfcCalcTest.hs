module Main (main) where

import Test.HUnit(runTestTT, Counts, Test (TestList))
import qualified SuitTest as SuitTests

main :: IO Counts
main = runTestTT $ TestList SuitTests.result
