-- | User board and its processing methods
module Game.Board where

import CardParts.Cards ( Card (..), parseCard )
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))
import Data.Maybe (isNothing, mapMaybe)

-- | Parsed card result wrapper which also holds the string arg
type CardParseResult = (String, Maybe Card)
-- | Shorthand for board type
type Board = [[Card]]

{- | This method gets a list of strings which items represents a card notation.
Returns 'Either' type, where 'Left' is 'String' with error message,
and 'Right' is 'Board' type.
Processes only list of length 13.

__Examples:__

@
cards = [
    "Ah", "Qd", "Kc",
    "Ts", "Jc", "6h", "2h", "3h",
    "8c", "4c", "7s", "9c", "Tc"
]
getUserBoard cards = Right [
    [
        Card {value = Ace, suit = Hearts},
        Card {value = Queen, suit = Diamonds},
        Card {value = King, suit = Clubs}
    ],
    [
        Card {value = Ten, suit = Spades},
        Card {value = Jack, suit = Clubs},
        Card {value = Six, suit = Hearts},
        Card {value = Two, suit = Hearts},
        Card {value = Three, suit = Hearts}
    ],
    [
        Card {value = Eight, suit = Clubs},
        Card {value = Four, suit = Clubs},
        Card {value = Seven, suit = Spades},
        Card {value = Nine, suit = Clubs},
        Card {value = Ten, suit = Clubs}
    ]
]

cardsTwo = [
    "Az", "Qx", "Kf",
    "Xs", "Jc", "6h", "2h", "3h",
    "8c", "4c", "7s", "9c", "Tc"
]
getUserBoard cardsTwo = Left "Failed to parse: Az; Qx; Kf; Xs;"
@
-}
getUserBoard :: [String] -> Either String Board
getUserBoard full
    | length full /= 13 = Left "List length should be 13"
    | not $ null failedCards = Left failedCardsErrMsg
    | otherwise = Right $ getLines $ mapMaybe snd parsedCards
    where
        -- | This is the predicate to check whether the parse failed
        isParseFailed :: (CardParseResult -> Bool)
        isParseFailed = \(str, strRes) -> isNothing strRes

        -- | Result of list of string card parsing, in form of a tuple
        parsedCards :: [CardParseResult]
        parsedCards = map (\str -> (str, parseCard str)) full

        -- | List of cards failed to being parsed
        failedCards :: [CardParseResult]
        failedCards = filter isParseFailed parsedCards

        -- | Err message in case of failed cards
        failedCardsErrMsg :: String
        failedCardsErrMsg = "Failed to parse: " ++ concat [ str ++ "; " | (str, strRes) <- failedCards ]

        -- | Get sublist with given number of elements from given starting point
        getSublist :: [a] -> Int -> Int -> [a]
        getSublist xs from count = take count $ drop from xs

        -- | Split list of cards into three lines
        getLines :: [Card] -> Board
        getLines [] = []
        getLines xs = [getSublist xs 0 3, getSublist xs 3 5, getSublist xs 8 5]
        