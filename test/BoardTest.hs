module BoardTest(result) where

import TestOps(generateTests)
import Test.HUnit (Test)
import Game.Board (Board, getUserBoard)
import CardParts.Cards (Card(..))
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))

testData :: [([Char], [String], Either String Board)]
testData = [
        (
            "Failed #1: Length is not 13",
            ["Ac"],
            Left "List length should be 13"
        ),
        (
            "Failed #2: Dups ocurred",
            [
                "Ac", "Ad", "Ac",
                "Js", "Jc", "Js", "5d", "6d",
                "8d", "9d", "Tc", "Tc", "2h"
            ],
            Left "Duplicates ocurred: Ac; Js; Tc; "
        ),
        (
            "Failed #3: Some cards failed to be parsed",
            [
                "Az", "1d", "As",
                "Js", "Jc", "Jh", "5d", "6d",
                "8d", "9d", "Tc", "Th", "2h"
            ],
            Left "Some cards failed to be parsed: There is no card suit marked as 'z'; There is no number card with value 1; "
        ),
        (
            "Success",
            [
                "Ah", "2d", "As",
                "Js", "Jc", "Jh", "5d", "6d",
                "8d", "9d", "Tc", "Th", "2h"
            ],
            Right [
                [
                    Card {value = Ace, suit = Hearts },
                    Card {value = Two, suit = Diamonds },
                    Card {value = Ace, suit = Spades }
                ],
                [
                    Card {value = Jack, suit = Spades },
                    Card {value = Jack, suit = Clubs },
                    Card {value = Jack, suit = Hearts },
                    Card {value = Five, suit = Diamonds },
                    Card {value = Six, suit = Diamonds }
                ],
                [
                    Card {value = Eight, suit = Diamonds },
                    Card {value = Nine, suit = Diamonds },
                    Card {value = Ten, suit = Clubs },
                    Card {value = Ten, suit = Hearts },
                    Card {value = Two, suit = Hearts }
                ]
            ]
        )
    ]

result :: [Test]
result = generateTests getUserBoard "getUserBoard" testData
