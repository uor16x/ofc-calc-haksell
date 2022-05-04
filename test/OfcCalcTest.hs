module Main (main) where

import Test.HUnit(runTestTT, Counts, Test (TestList))
import qualified SuitTest
import qualified ValueTest
import qualified CardTest

main :: IO Counts
main = runTestTT $ TestList $ ValueTest.result ++ SuitTest.result ++ CardTest.result
