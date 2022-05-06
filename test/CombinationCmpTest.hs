module CombinationCmpTest(result) where

import TestOps(generateTests)
import Game.Combination(Combination(..), CombinationName (..))
import Test.HUnit (Test)
import CardParts.Cards (Card(..))
import CardParts.Suits (Suit(..))
import CardParts.Values (Value(..))

testDataPairFours :: [(String, Combination, Ordering)]
testDataPairFours = [
        (
            "Pair",
            RankCombination Pair Card { value = Four, suit = Spades },
            EQ
        )
    ]

result :: [Test]
result =
    generateTests
    ( RankCombination Pair Card { value = Four, suit = Spades } `compare`)
    "RankCombination Pair Card { value = Four, suit = Spades } compare"
    testDataPairFours
    ++
    generateTests
    ( RankCombination Pair Card { value = Four, suit = Spades } `compare`)
    "RankCombination Pair Card { value = Four, suit = Spades } compare"
    testDataPairFours
