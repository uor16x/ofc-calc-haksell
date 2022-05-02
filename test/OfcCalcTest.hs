module Main (main) where

import Test.HUnit(runTestTT, Counts)
import qualified SuitTest as SuitTests

main :: IO Counts
main = runTestTT SuitTests.result
