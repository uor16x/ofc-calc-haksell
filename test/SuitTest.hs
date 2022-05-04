module SuitTest(result) where

import CardParts.Suits ( parseSuit, Suit(..) )
import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts,
      Test(TestLabel, TestCase) )

testData :: [([Char], Char, Either String Suit)]
testData = [
        ("Should return hearts", 'h', Right Hearts),
        ("Should return diamonds", 'd', Right Diamonds),
        ("Should return clubs", 'c', Right Clubs),
        ("Should return spades", 's', Right Spades),
        ("Should return nothing #1", 'q', Left "There is no card suit marked as 'q'"),
        ("Should return nothing #2", '1', Left "There is no card suit marked as '1'"),
        ("Should return nothing #3", '-', Left "There is no card suit marked as '-'")
    ]

generateTests :: String -> [Test]
generateTests method = [ TestLabel name $ TestCase(assertEqual (getDesc arg) expected (parseSuit arg)) | (name, arg, expected) <- testData ]
    where
        getDesc arg = method ++ " " ++ show arg

result :: [Test]
result = generateTests "parseSuit"
