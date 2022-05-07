module CombinationInnerTest where

import TestOps(generateTests)
import Game.Combination(Combination(..), CombinationName (..), getOccurrences, parsePartHand, parseSequence, parseCombination)
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

testParseSequence :: [(String, [Card], Either String Combination)]
testParseSequence = [
    (
        "Empty list",
        [],
        Left "Can't process empty list"
    ),
    (
        "Straight",
        [
            Card Four Spades,
            Card Five Spades,
            Card Six Clubs,
            Card Seven Diamonds,
            Card Eight Hearts
        ],
        Right $ RankCombination Straight $ Card Eight Hearts
    ),
    (
        "Wheel",
        [
            Card Two Diamonds,
            Card Three Spades,
            Card Five Spades,
            Card Ace Diamonds,
            Card Four Clubs
        ],
        Right $ RankCombination Straight $ Card Five Spades
    ),
    (
        "Flush",
        [
            Card Four Spades,
            Card Seven Spades,
            Card Nine Spades,
            Card Jack Spades,
            Card King Spades
        ],
        Right $ RankCombination Flush $ Card King Spades
    ),
    (
        "Straight flush",
        [
            Card Four Spades,
            Card Five Spades,
            Card Six Spades,
            Card Seven Spades,
            Card Eight Spades
        ],
        Right $ RankCombination StraightFlush $ Card Eight Spades
    ),
    (
        "Royal flush",
        [
            Card Ten Hearts,
            Card Jack Hearts,
            Card Queen Hearts,
            Card King Hearts,
            Card Ace Hearts
        ],
        Right $ RankCombination RoyalFlush $ Card Ace Hearts
    ),
    (
        "Straight flush - wheel",
        [
            Card Ace Diamonds,
            Card Four Diamonds,
            Card Two Diamonds,
            Card Three Diamonds,
            Card Five Diamonds
        ],
        Right $ RankCombination StraightFlush $ Card Five Diamonds
    ),
    (
        "Kicker #1",
        [
            Card Ten Diamonds,
            Card Six Spades,
            Card Five Hearts
        ],
        Right $ RankCombination Kicker $ Card Ten Diamonds
    ),
    (
        "Kicker #2",
        [
            Card King Diamonds,
            Card Eight Spades,
            Card Two Hearts,
            Card Six Clubs,
            Card Seven Clubs
        ],
        Right $ RankCombination Kicker $ Card King Diamonds
    ),
    (
        "Kicker #3",
        [
            Card Four Diamonds,
            Card Six Spades,
            Card Five Hearts
        ],
        Right $ RankCombination Kicker $ Card Six Spades
    ),
    (
        "Kicker #4",
        [
            Card Four Diamonds,
            Card Eight Diamonds,
            Card Five Diamonds
        ],
        Right $ RankCombination Kicker $ Card Eight Diamonds
    ),
    (
        "Kicker #5",
        [
            Card Four Diamonds,
            Card Six Diamonds,
            Card Five Diamonds
        ],
        Right $ RankCombination Kicker $ Card Six Diamonds
    )
    ]

testParseCombination = [
    (
        "Invalid line length #1",
        [
            Card Ace Spades,
            Card King Spades
        ],
        Left "Invalid line length: 2"
    ),
    (
        "Kicker #1",
        [
            Card Jack Spades,
            Card King Hearts,
            Card Ten Diamonds
        ],
        Right $ RankCombination Kicker $ Card King Hearts
    ),
    (
        "Kicker #2",
        [
            Card Four Spades,
            Card Three Hearts,
            Card Two Spades
        ],
        Right $ RankCombination Kicker $ Card Four Spades
    ),
    (
        "Kicker #3",
        [
            Card Five Spades,
            Card Six Spades,
            Card Seven Hearts
        ],
        Right $ RankCombination Kicker $ Card Seven Spades
    ),
    (
        "Kicker #4",
        [
            Card Five Spades,
            Card Six Spades,
            Card Eight Spades
        ],
        Right $ RankCombination Kicker $ Card Eight Spades
    ),
    (
        "Kicker #5",
        [
            Card Five Spades,
            Card Six Spades,
            Card Seven Spades
        ],
        Right $ RankCombination Kicker $ Card Seven Spades
    ),
    (
        "Pair #1",
        [
            Card Nine Hearts,
            Card Nine Clubs,
            Card Ten Spades
        ],
        Right $ RankCombination Pair $ Card Nine Hearts
    ),
    (
        "Pair #2",
        [
            Card Six Spades,
            Card Eight Hearts,
            Card Ace Spades,
            Card Eight Spades,
            Card Nine Clubs
        ],
        Right $ RankCombination Pair $ Card Eight Hearts
    ),
    (
        "Two Pairs",
        [
            Card King Spades,
            Card Ace Hearts,
            Card Five Diamonds,
            Card Five Hearts,
            Card King Clubs
        ],
        Right $ PartCombination TwoPairs (Card King Hearts) (Card Five Diamonds)
    ),
    (
        "Set #1",
        [
            Card Jack Spades,
            Card Jack Diamonds,
            Card Jack Hearts
        ],
        Right $ RankCombination Set $ Card Jack Spades
    ),
    (
        "Set #2",
        [
            Card Four Hearts,
            Card Four Diamonds,
            Card Six Hearts,
            Card Seven Spades,
            Card Four Clubs
        ],
        Right $ RankCombination Set $ Card Four Hearts
    ),
    (
        "Straight",
        [
            Card Six Spades,
            Card Seven Spades,
            Card Eight Clubs,
            Card Nine Diamonds,
            Card Ten Hearts
        ],
        Right $ RankCombination Straight $ Card Ten Hearts
    ),
    (
        "Wheel",
        [
            Card Two Clubs,
            Card Three Hearts,
            Card Five Spades,
            Card Ace Hearts,
            Card Four Diamonds
        ],
        Right $ RankCombination Straight $ Card Five Spades
    ),
    (
        "Flush",
        [
            Card Two Clubs,
            Card Four Clubs,
            Card Seven Clubs,
            Card Queen Clubs,
            Card Jack Clubs
        ],
        Right $ RankCombination Flush $ Card Queen Clubs
    ),
    (
        "Full House",
        [
            Card Seven Clubs,
            Card Seven Spades,
            Card Five Hearts,
            Card Five Clubs,
            Card Seven Hearts
        ],
        Right $ PartCombination FullHouse (Card Seven Clubs) (Card Five Hearts)
    ),
    (
        "Four of a kind",
        [
            Card Nine Hearts,
            Card Nine Diamonds,
            Card Ace Spades,
            Card Nine Clubs,
            Card Nine Spades
        ],
        Right $ RankCombination FourOfAKind $ Card Nine Hearts
    ),
    (
        "Straight flush",
        [
            Card Seven Spades,
            Card Eight Spades,
            Card Jack Spades,
            Card Ten Spades,
            Card Nine Spades
        ],
        Right $ RankCombination StraightFlush $ Card Jack Spades
    ),
    (
        "Straight flush - wheel",
        [
            Card Ace Diamonds,
            Card Four Diamonds,
            Card Two Diamonds,
            Card Three Diamonds,
            Card Five Diamonds
        ],
        Right $ RankCombination StraightFlush $ Card Five Diamonds
    ),
    (
        "Royal flush",
        [
            Card Ten Spades,
            Card Jack Spades,
            Card Ace Spades,
            Card Queen Spades,
            Card King Spades
        ],
        Right $ RankCombination RoyalFlush $ Card Ace Spades
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
    ++
    generateTests
    parseSequence
    "parseSequence"
    testParseSequence
    ++
    generateTests
    parseCombination
    "parseCombination"
    testParseCombination

