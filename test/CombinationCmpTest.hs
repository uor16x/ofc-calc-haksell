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
            "Pair of fours COMPARE Same combination",
            RankCombination Pair Card { value = Four, suit = Spades },
            EQ
        ),
        (
            "Pair of fours COMPARE Same combination, other suit",
            RankCombination Pair Card { value = Four, suit = Hearts },
            EQ
        ),
        (
            "Pair of fours COMPARE Pair of 6",
            RankCombination Pair Card { value = Six, suit = Spades },
            LT
        ),
        (
            "Pair of fours COMPARE Set of T",
            RankCombination Pair Card { value = Ten, suit = Spades },
            LT
        ),
        (
            "Pair of fours COMPARE Two pairs: T",
            PartCombination TwoPairs Card { value = Ten, suit = Spades } Card { value = Ten, suit = Hearts },
            LT
        ),
        (
            "Pair of fours COMPARE Full House QQQJJ",
            PartCombination FullHouse Card { value = Queen, suit = Clubs } Card { value = Jack, suit = Diamonds },
            LT
        ),
        (
            "Pair of fours COMPARE pair of deuces",
            RankCombination Pair Card { value = Two, suit = Clubs },
            GT
        )
    ]

testDataFullHouseNinesAndEights :: [(String, Combination, Ordering)]
testDataFullHouseNinesAndEights = [
        (
            "FullHouse 99988 COMPARE Same combination",
            PartCombination FullHouse Card { value = Nine, suit = Clubs } Card {value = Eight, suit = Hearts },
            EQ
        ),
        (
            "FullHouse 99988 COMPARE Same combination, other suit",
            PartCombination FullHouse Card { value = Nine, suit = Clubs } Card {value = Eight, suit = Spades },
            EQ
        ),
        (
            "FullHouse 99988 COMPARE Pair of 6",
            RankCombination Pair Card { value = Six, suit = Spades },
            GT
        ),
        (
            "FullHouse 99988 COMPARE Set of T",
            RankCombination Pair Card { value = Ten, suit = Spades },
            GT
        ),
        (
            "FullHouse 99988 COMPARE Two pairs: T",
            PartCombination TwoPairs Card { value = Ten, suit = Spades } Card { value = Ten, suit = Hearts },
            GT
        ),
        (
            "FullHouse 99988 COMPARE Full House QQQJJ",
            PartCombination FullHouse Card { value = Queen, suit = Clubs } Card { value = Jack, suit = Diamonds },
            LT
        ),
        (
            "FullHouse 99988 COMPARE Royal Flush",
            RankCombination RoyalFlush Card { value = Ace, suit = Clubs },
            LT
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
    ( PartCombination FullHouse Card { value = Nine, suit = Clubs } Card {value = Eight, suit = Hearts } `compare`)
    "PartCombination FullHouse Card { value = Nine, suit = Clubs } Card {value = Eight, suit = Hearts } compare"
    testDataFullHouseNinesAndEights
