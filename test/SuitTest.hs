module SuitTest(result) where

import CardParts.Suits ( parseSuit, Suit(..) )
import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts,
      Test(TestLabel, TestCase) )

testData :: [([Char], Char, Maybe Suit)]
testData = [
        ("Should return hearts", 'h', Just Hearts),
        ("Should return diamonds", 'd', Just Diamonds),
        ("Should return clubs", 'c', Just Clubs),
        ("Should return spades", 's', Just Spades),
        ("Should return nothing #1", 'q', Nothing),
        ("Should return nothing #2", '1', Nothing),
        ("Should return nothing #3", '-', Nothing)
    ]

generateTests :: String -> [Test]
generateTests method = [ TestLabel name $ TestCase(assertEqual (getDesc arg) expected (parseSuit arg)) | (name, arg, expected) <- testData ]
    where
        getDesc arg = method ++ " " ++ show arg

result :: [Test]
result = generateTests "parseSuit"
