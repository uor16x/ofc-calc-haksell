module CardTest(result) where

import CardParts.Cards ( parseCard, Card(..) )
import CardParts.Values ( Value(..) )
import CardParts.Suits ( Suit(..) )
import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts,
      Test(TestLabel, TestCase) )
import Data.Char (intToDigit)

{- | Generates a list of tuples.
Each tuple contains:
 * description
 * card notation
 * 'Right' 'Card' parsed from notation
With all possible cards and with all possible card notations.
Also a few invalid examples attached to the end of the list.

__Result looks the following way:__

@
("Should be Card { value = 'Two', suit = 'Hearts' }", '"2h", 'Just' 'Card' { value = 'Two', suit = 'Hearts' })
("Should be Card { value = 'Two', suit = 'Diamonds' }", '"2d", 'Just' 'Card' { value = 'Two', suit = 'Diaminds' })
...
("Should be Card { value = 'Ace', suit = 'Spades' }", '"As", 'Just' 'Card' { value = 'Ace', suit = 'Spades' })
("Should be 'Left' #1", '"2x", 'Nothing')
...
-}
testData :: [([Char], String, Either String Card)]
testData
    | length allCards /= length allStrings = error "Lengths of cards and strings should be the same"
    | otherwise =
        [ ("Should be " ++ show (allCards !! index), allStrings !! index, Right $ allCards !! index) | index <- [0 .. (length allCards - 1)] ]
        ++
        [ 
            ("Should be Left #1", "2x", Left "There is no card suit marked as 'x'"),
            ("Should be Left #2", "1x", Left "There is no number card with value 1"),
            ("Should be Left #3", "Xc", Left "There is no broadway card, which could be represented with 'X'"),
            ("Should be Left #4", "Tz", Left "There is no card suit marked as 'z'"),
            ("Should be Left #5", "Ab", Left "There is no card suit marked as 'b'"),
            ("Should be Left #6", "aB", Left "There is no broadway card, which could be represented with 'a'"),
            ("Should be Left #7", "a2", Left "There is no broadway card, which could be represented with 'a'"),
            ("Should be Left #8", "ac", Left "There is no broadway card, which could be represented with 'a'"),
            ("Should be Left #9", "8D", Left "There is no card suit marked as 'D'"),
            ("Should be Left #10", "AsC", Left "Argument length should be 2"),
            ("Should be Left #11", "", Left "Can't process emtpy string")
        ]
    where
        values = [Two .. Ace]
        suits = [Hearts .. Spades]
        valueSymbols = [ intToDigit d | d <- [2..9] ] ++ "TJQKA"
        suitSymbols = "hdcs"
        allCards = [ Card { value = v, suit = s } | v <- values, s <- suits ]
        allStrings = [ [v, s] | v <- valueSymbols, s <- suitSymbols ]

generateTests :: String -> [Test]
generateTests method = [ TestLabel name $ TestCase(assertEqual (getDesc arg) expected (parseCard arg)) | (name, arg, expected) <- testData ]
    where
        getDesc arg = method ++ " " ++ show arg

result :: [Test]
result = generateTests "parseCard"
