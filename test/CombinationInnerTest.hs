module CombinationInnerTest where

import TestOps(generateTests)
import Game.Combination(Combination(..), CombinationName (..), getOccurrences, parsePartHand)
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

testParsePartHand :: [(String, [(Card, Int)], Either String Combination)]
testParsePartHand = [
    (
        "Fail for empty",
        [],
        Left "Can't process empty pairs array"
    ),
    (
        "Invalid number of pairs",
        [
            (Card Ace Spades, 2),
            (Card King Spades, 2),
            (Card Queen Spades, 2)
        ],
        Left "Invalid number of pairs: 3"
    ),
    (
        "Single pair hand: invalid number of pairs",
        [
            (Card Ace Spades, 5)
        ],
        Left "Single pair hand: found invalid number of pairs: 5"
    ),
    (
        "Single pair hand: counter == 4",
        [
            (Card Ace Spades, 4)
        ],
        Right $ RankCombination FourOfAKind $ Card Ace Spades
    ),
    (
        "Single pair hand: counter == 3",
        [
            (Card Ace Spades, 3)
        ],
        Right $ RankCombination Set $ Card Ace Spades
    ),
    (
        "Single pair hand: counter == 2",
        [
            (Card Ace Spades, 2)
        ],
        Right $ RankCombination Pair $ Card Ace Spades
    ),
    (
        "Multiple pairs hand: counters sum == invalid",
        [
            (Card Ace Spades, 4),
            (Card Ace Spades, 4)
        ],
        Left "Invalid multiple pairs sum: 8"
    ),
    (
        "Multiple pairs hand: counters sum == 4",
        [
            (Card Ace Spades, 2),
            (Card King Spades, 2)
        ],
        Right $ PartCombination TwoPairs (Card Ace Spades) (Card King Spades)
    ),
    (
        "Multiple pairs hand: counters sum == 5",
        [
            (Card Ace Spades, 3),
            (Card King Spades, 2)
        ],
        Right $ PartCombination FullHouse (Card Ace Spades) (Card King Spades)
    )
    ]

result :: [Test]
result =
    generateTests
    getOccurrences
    "getOccurrences"
    testOccurences
    ++
    generateTests
    parsePartHand
    "parsePartHand"
    testParsePartHand


