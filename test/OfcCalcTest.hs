module Main (main) where
import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts,
      Test(TestLabel, TestCase, TestList) )
import OfcCalc (Suit(..), parseSuit)

-- TODO: auto generation
parseSuitH = TestCase(assertEqual "parseSuit 'h'" (Just Hearts) (parseSuit 'h'))
parseSuitD = TestCase(assertEqual "parseSuit 'd'" (Just Diamonds) (parseSuit 'd'))
parseSuitC = TestCase(assertEqual "parseSuit 'c'" (Just Clubs) (parseSuit 'c'))
parseSuitS = TestCase(assertEqual "parseSuit 's'" (Just Spades) (parseSuit 's'))

tests = TestList [TestLabel "test1" parseSuitS, TestLabel "test2" parseSuitD, TestLabel "test3" parseSuitC, TestLabel "test4" parseSuitS]

main :: IO Counts
main = runTestTT tests
