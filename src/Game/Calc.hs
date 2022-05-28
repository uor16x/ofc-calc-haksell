module Game.Calc() where
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
    top :: Maybe LineResult,
    middle :: Maybe LineResult,
    bottom :: Maybe LineResult
}

calcPlayer :: PlayerInput -> PlayerCalculations
calcPlayer input@PlayerInput{ board = (top:middle:bottom:xs) } = PlayerCalculations {
    player = input,
    top = if isScoopped then Nothing else Just (getLineResult Top top),
    middle = if isScoopped then Nothing else Just (getLineResult Middle middle),
    bottom = if isScoopped then Nothing else Just (getLineResult Top top)
} where
    -- | Define whether the user scooped
    isScoopped :: Bool
    isScoopped = top > middle || middle > bottom
calcPlayer input@PlayerInput { board = _ } = PlayerCalculations{
    player = input,
    top = Nothing,
    middle = Nothing,
    bottom = Nothing
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
