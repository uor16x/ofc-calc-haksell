module Game.Calc where
import CardParts.Cards (Card(..))
import Game.Combination
    ( Combination,
      CombinationName(..),
      Combination(RankCombination),
      Combination(..) )
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))

data PlayerInput = PlayerInput {
    username :: String,
    board :: [Combination]
}

data LineType = Top | Middle | Bottom

data LineResult = LineResult {
    lineType :: LineType,
    combination :: Combination,
    points :: Int
}

data PlayerCalculations = PlayerCalculations {
    player :: PlayerInput,
    top :: LineResult,
    middle :: LineResult,
    bottom :: LineResult
}

calc :: PlayerInput -> PlayerCalculations
calc input@PlayerInput{ board = board } = PlayerCalculations {
    player = input,
    top = getLineResult Top (head board),
    middle = getLineResult Top (head $ tail board),
    bottom = getLineResult Top (last board)
}

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
