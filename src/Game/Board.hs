-- | User board and its processing methods
module Game.Board where

import CardParts.Cards ( Card (..), parseCard )
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))
import Data.Either (isLeft)
import Data.List ( nub, elemIndex )

-- | Parsed card result wrapper which also holds the possible err msg
type CardParseResult = Either String Card
-- | Shorthand for board type
type Board = [[Card]]

{- | This method gets a list of strings which items represents a card notation.
Returns 'Either' type, where 'Left' is 'String' with error message,
and 'Right' is 'Board' type.
Processes only list of length 13.
Method is going to fail the list with duplicates.

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
getUserBoard cardsTwo = Left
    "Some cards failed to be parsed:
    There is no card suit marked as 'z';
    There is no card suit marked as 'x';
    There is no card suit marked as 'f';
    There is no broadway card, which could be represented with 'X';"
@
-}
getUserBoard :: [String] -> Either String Board
getUserBoard full
    | length full /= 13 = Left "List length should be 13"
    | not $ null boardDuplicates = Left $ "Duplicates ocurred: " ++ boardDuplicates
    | not $ null failedCards = Left failedCardsErrMsg
    | otherwise = Right $ getLines [ card | Right card <- parsedCards ]
    where
        -- | Function which calculates list duplicates
        getDuplicates :: [String] -> [String]
        getDuplicates [] = []
        getDuplicates (x:xs) = case x `elemIndex` xs of
            Nothing -> getDuplicates xs
            Just el -> nub $ x : getDuplicates xs

        -- | Shorthand for list duplicates
        boardDuplicates :: String
        boardDuplicates = concatMap (++"; ") $ getDuplicates full

        -- | Function which calculates whether list contains duplicates
        containsDuplicates :: [String] -> Bool
        containsDuplicates xs = length xs /= length ( nub xs )

        -- | Result of list of string card parsing, in form of a tuple
        parsedCards :: [CardParseResult]
        parsedCards = map parseCard full

        -- | List of cards failed to being parsed
        failedCards :: [CardParseResult]
        failedCards = filter isLeft parsedCards

        -- | Err message in case of failed cards
        failedCardsErrMsg :: String
        failedCardsErrMsg = "Some cards failed to be parsed: "
            ++ concat [ errMsg ++ "; " | Left errMsg <- failedCards ]

        -- | Get sublist with given number of elements from given starting point
        getSublist :: [a] -> Int -> Int -> [a]
        getSublist xs from count = take count $ drop from xs

        -- | Split list of cards into three lines
        getLines :: [Card] -> Board
        getLines [] = []
        getLines xs = [getSublist xs 0 3, getSublist xs 3 5, getSublist xs 8 5]
        