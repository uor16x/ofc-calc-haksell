module CombinationInnerTest where

import TestOps(generateTests)
import Game.Combination(Combination(..), CombinationName (..), getOccurrences)
import Test.HUnit (Test)
import CardParts.Cards (Card (..))
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))

testOccurences :: [(String, [Card], [(Card, Int)])]
testOccurences = [
        (
            "Passed [] is []",
            [],
            []
        ),
        (
            "Happy path #1",
            [
                Card Two Hearts,
                Card Three Clubs,
                Card Four Diamonds
            ],
            [
                (Card Two Hearts, 1),
                (Card Three Clubs, 1),
                (Card Four Diamonds, 1)
            ]
        ),
        (
            "Happy path #2",
            [
                Card Two Hearts,
                Card Three Clubs,
                Card Four Diamonds,
                Card Two Clubs,
                Card Three Diamonds
            ],
            [
                (Card Two Hearts, 2),
                (Card Three Clubs, 2),
                (Card Four Diamonds, 1)
            ]
        ),
        (
            "Happy path #3",
            [
                Card Two Hearts,
                Card Three Clubs,
                Card Four Diamonds,
                Card Two Clubs,
                Card Three Clubs
            ],
            [
                (Card Two Hearts, 2),
                (Card Three Clubs, 2),
                (Card Four Diamonds, 1)
            ]
        ),
        (
            "Happy path #3",
            [
                Card Two Hearts,
                Card Two Diamonds,
                Card Two Clubs,
                Card Two Spades,
                Card Ace Hearts
            ],
            [
                (Card Two Hearts, 4),
                (Card Ace Hearts, 1)
            ]
        ),
        (
            "Happy path #4",
            [
                Card Two Hearts,
                Card Two Diamonds,
                Card Four Clubs,
                Card Four Spades,
                Card Ace Hearts
            ],
            [
                (Card Two Hearts, 2),
                (Card Four Clubs, 2),
                (Card Ace Hearts, 1)
            ]
        )
    ]

result :: [Test]
result =
    generateTests
    getOccurrences
    "getOccurrences"
    testOccurences


