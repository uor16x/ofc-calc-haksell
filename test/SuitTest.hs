module SuitTest(result) where

import TestOps(generateTests)
import Test.HUnit (Test)
import CardParts.Suits ( parseSuit, Suit(..) )

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

result :: [Test]
result = generateTests parseSuit "parseSuit" testData
