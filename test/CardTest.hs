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
 * 'Just' 'Card' parsed from notation
With all possible cards and with all possible card notations.
Also a few Nothing examples attached to the end of the list.

__Result looks the following way:__

@
("Should be Card { value = 'Two', suit = 'Hearts' }", '"2h", 'Just' 'Card' { value = 'Two', suit = 'Hearts' })
("Should be Card { value = 'Two', suit = 'Diamonds' }", '"2d", 'Just' 'Card' { value = 'Two', suit = 'Diaminds' })
...
("Should be Card { value = 'Ace', suit = 'Spades' }", '"As", 'Just' 'Card' { value = 'Ace', suit = 'Spades' })
("Should be Nothing #1", '"2x", 'Nothing')
...
-}
testData :: [([Char], String, Maybe Card)]
testData
    | length allCards /= length allStrings = error "Lengths of cards and strings should be the same"
    | otherwise =
        [ ("Should be " ++ show (allCards !! index), allStrings !! index, Just $ allCards !! index) | index <- [0 .. (length allCards - 1)] ]
        ++
        [ ("Should be nothing #" ++ show index, invalidSymbols !! index, Nothing) | index <- [0 .. length invalidSymbols - 1] ]
    where
        values = [Two .. Ace]
        suits = [Hearts .. Spades]
        valueSymbols = [ intToDigit d | d <- [2..9] ] ++ "TJQKA"
        suitSymbols = "hdcs"
        allCards = [ Card { value = v, suit = s } | v <- values, s <- suits ]
        allStrings = [ [v, s] | v <- valueSymbols, s <- suitSymbols ]
        invalidSymbols = ["2x", "1x", "Xc", "Tz", "Ab", "aB", "a2", "ac", "8D"]

generateTests :: String -> [Test]
generateTests method = [ TestLabel name $ TestCase(assertEqual (getDesc arg) expected (parseCard arg)) | (name, arg, expected) <- testData ]
    where
        getDesc arg = method ++ " " ++ show arg

result :: [Test]
result = generateTests "parseCard"
