{-# LANGUAGE DeriveGeneric #-}
module Game.Calc(calcPlayer, PlayerInput(..), LineResult, LineType(..), PlayerCalculations(..)) where
import CardParts.Cards (Card(..))
import Game.Combination
    ( Combination,
      CombinationName(..),
      Combination(RankCombination),
      Combination(..) )
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data PlayerInput = PlayerInput {
    username :: String,
    board :: [Combination]
} deriving (Show, Generic)

instance FromJSON PlayerInput
instance ToJSON PlayerInput

data LineType = Top | Middle | Bottom deriving (Show, Generic)

instance FromJSON LineType
instance ToJSON LineType

data LineResult = LineResult {
    lineType :: LineType,
    combination :: Combination,
    points :: Int
} deriving (Show, Generic)

instance FromJSON LineResult
instance ToJSON LineResult

data PlayerCalculations = PlayerCalculations {
    player :: PlayerInput,
    top :: LineResult,
    middle :: LineResult,
    bottom :: LineResult
} deriving (Generic)

instance FromJSON PlayerCalculations
instance ToJSON PlayerCalculations

calcPlayer :: PlayerInput -> PlayerCalculations
calcPlayer input@PlayerInput{ board = (top:middle:bottom:xs) } = PlayerCalculations {
    player = input,
    top = getLineResult Top top,
    middle = getLineResult Middle middle,
    bottom = getLineResult Bottom bottom
}
calcPlayer _ = error "Invalid usage of calcPlayer"

getLineResult :: LineType -> Combination -> LineResult
getLineResult lineType combination = LineResult {
    lineType = lineType,
    combination = combination,
    points = getPoints lineType combination
}

middlePoints :: [Int]
middlePoints = [0, 0, 0, 2, 4, 8, 12, 20, 30, 50]

bottomPoints :: [Int]
bottomPoints = map (\p -> if p > 2 then p `div` 2 else 0) middlePoints

getPoints :: LineType -> Combination -> Int
getPoints Top (RankCombination Pair (Card rank _)) =
    if fromEnum rank < 4 then 0 else fromEnum rank - 3
getPoints Top (RankCombination Set (Card rank _)) = fromEnum rank + 10
getPoints Top _ = 0
getPoints Middle c = middlePoints !! fromEnum (name c)
getPoints Bottom c = bottomPoints !! fromEnum (name c)
