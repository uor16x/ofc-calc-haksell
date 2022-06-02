{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use first" #-}
module Game.Calc(PlayerInput(..), LineResult, LineType(..), PlayerCalculations(..), comparePlayers, calcGame) where
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
import Data.List (find)

data PlayerInput = PlayerInput {
    username :: String,
    board :: [Combination],
    scoop :: Bool,
    withFantasy :: Bool
} deriving (Show, Generic)

instance FromJSON PlayerInput
instance ToJSON PlayerInput

data LineType = Top | Middle | Bottom deriving (Show, Generic)

instance FromJSON LineType
instance ToJSON LineType

type LineCompareResult = ([String], Int, Int, [(Int, Int)])

data LineResult = LineResult {
    lineType :: LineType,
    combination :: Maybe Combination,
    points :: Int,
    totalCombination :: Int,
    totalBonus :: Int
} deriving (Show, Generic)

instance FromJSON LineResult
instance ToJSON LineResult

data PlayerCalculations = PlayerCalculations {
    player :: PlayerInput,
    top :: LineResult,
    middle :: LineResult,
    bottom :: LineResult,
    isScoop :: Bool,
    isNextFantasy :: Bool,
    totalDetailed :: (String, [LineCompareResult]),
    total :: Int
} deriving (Generic)

instance FromJSON PlayerCalculations
instance ToJSON PlayerCalculations

type IntPair = (Int, Int)

calcGame :: [PlayerInput] -> [PlayerCalculations]
calcGame playerInputs
  | length playerInputs /= 3 = error "Invalid number of players"
  | length linesResults /= 3 = error "Invalid number of linesResults"
  | otherwise = map mapLinesResults [0..2]
  where
    linesResults :: [(String, [LineCompareResult])]
    linesResults = collectLinesResults [] playerInputs playerInputs

    mapLinesResults :: Int -> PlayerCalculations
    mapLinesResults index = PlayerCalculations {
      player = playerInputs !! index,
      isScoop = scoop $ playerInputs !! index,
      isNextFantasy =
        not (scoop (playerInputs !! index)) &&
        nextIsFantasy
        (withFantasy $ playerInputs !! index)
        (board $ playerInputs !! index),
      top = LineResult {
        combination = combinationByIndex (playerInputs !! index) 0, -- top line has index 0,
        lineType = Top,
        points = pointsCalc Top (combinationByIndex (playerInputs !! index) 0),
        totalCombination = fst $ getLinePoints index 0,
        totalBonus = snd $ getLinePoints index 0
      },
      middle = LineResult {
        combination = combinationByIndex (playerInputs !! index) 1, -- middle line has index 1,
        lineType = Middle,
        points = pointsCalc Middle (combinationByIndex (playerInputs !! index) 1),
        totalCombination = fst $ getLinePoints index 1,
        totalBonus = snd $ getLinePoints index 1
      },
      bottom = LineResult {
        combination = combinationByIndex (playerInputs !! index) 2, -- bottom line has index 2,
        lineType = Bottom,
        points = pointsCalc Bottom (combinationByIndex (playerInputs !! index) 2),
        totalCombination = fst $ getLinePoints index 2,
        totalBonus = snd $ getLinePoints index 2
      },
      totalDetailed = linesResults !! index,
      total = totalPoints (linesResults !! index)
      }

    combinationByIndex :: PlayerInput -> Int -> Maybe Combination
    combinationByIndex playerInput index
      | scoop playerInput = Nothing
      | otherwise = Just $ board playerInput !! index

    getTotal :: LineCompareResult -> Int
    getTotal (_, combo, bonus, _) = combo + bonus

    totalPoints ::  (String, [LineCompareResult]) -> Int
    totalPoints (_, compareResults) = foldl (\acc r -> acc + getTotal r) 0 compareResults

    nextIsFantasy :: Bool -> [Combination] -> Bool
    nextIsFantasy withFantasy [] = False
    nextIsFantasy withFantasy (top:middle:bottom:_)
      | withFantasy =
        top >= RankCombination Set (Card Two Hearts)
        || middle >= PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)
        || bottom >= RankCombination FourOfAKind (Card Two Hearts)
      | otherwise =
        top >= RankCombination Pair (Card Queen Hearts)
        || middle >= PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)
        || bottom >= RankCombination StraightFlush (Card Five Hearts)
    nextIsFantasy _ _ = False


    getLinePoints :: Int -> Int -> IntPair
    getLinePoints playerIndex lineIndex
      = foldl
        (\(comboAcc, bonusAcc) (_, _, _, debug) -> (comboAcc + fst (debug !! lineIndex), bonusAcc + snd (debug !! lineIndex)))
        (0, 0)
        (snd (linesResults !! playerIndex))

collectLinesResults :: [LineCompareResult] -> [PlayerInput] -> [PlayerInput] -> [(String, [LineCompareResult])]
collectLinesResults acc full (currPlayer:players) =
  (username currPlayer, map (getResult currPlayer) otherInputs) : collectLinesResults (acc ++ map (getResult currPlayer) otherInputs) full players
    where
      otherInputs :: [PlayerInput]
      otherInputs = filter (\player -> username player /= username currPlayer) full

      negateTuple :: (Int, Int) -> (Int, Int)
      negateTuple (a, b) = (negate a, negate b)

      getResult :: PlayerInput -> PlayerInput -> ([String], Int, Int, [(Int, Int)])
      getResult player1 player2 = case find (\(usernames, _, _, _) -> usernames == [username player2, username player1] ) acc of
        Just (usernames, combo, bonus, debug) -> (reverse usernames, negate combo, negate bonus, map negateTuple debug)
        _ -> comparePlayers player1 player2
collectLinesResults acc _ [] = []

comparePlayers :: PlayerInput -> PlayerInput -> LineCompareResult
comparePlayers
  p1@PlayerInput{ username = p1name, board = boardP1, scoop = p1scoop }
  p2@PlayerInput{ username = p2name, board = boardP2, scoop = p2scoop } = (
    [p1name, p2name],
    foldPoints [topPoints, middlePoints, bottomPoints],
    bonusCalculated,
    [
      (uncurry (-) topPoints, topBonus),
      (uncurry (-) middlePoints, middleBonus),
      (uncurry (-) bottomPoints, bottomBonus)
    ]
  ) where
    topPoints :: IntPair
    topPoints = (
      if p1scoop then 0 else getPoints Top (head boardP1),
      if p2scoop then 0 else getPoints Top (head boardP2)
      )

    middlePoints :: IntPair
    middlePoints = (
      if p1scoop then 0 else getPoints Middle (head $ tail boardP1),
      if p2scoop then 0 else getPoints Middle (head $ tail boardP2)
      )

    bottomPoints :: IntPair
    bottomPoints = (
      if p1scoop then 0 else getPoints Bottom (last boardP1),
      if p2scoop then 0 else getPoints Bottom (last boardP2)
      )

    mirrorPoints :: Int -> (Int, Int)
    mirrorPoints p = (p, negate p)

    getDefaultBonus :: Combination -> Combination -> Int
    getDefaultBonus c1 c2
      | c1 > c2 = 1
      | c1 == c2 = 0
      | otherwise = -1

    getScoopBonus :: Int
    getScoopBonus = case (p1scoop, p2scoop) of
      (True, True) -> 0
      (True, False) -> -1
      (False, True) -> 1
      (False, False) -> error "getDefaultBonus shouldb be used for this case"

    anyScoop :: Bool
    anyScoop = p1scoop || p2scoop

    topBonus :: Int
    topBonus
      | anyScoop = getScoopBonus
      | otherwise = getDefaultBonus (head boardP1) (head boardP2)

    middleBonus :: Int
    middleBonus
      | anyScoop = getScoopBonus
      | otherwise = getDefaultBonus (head $ tail boardP1) (head $ tail boardP2)

    bottomBonus :: Int
    bottomBonus
      | anyScoop = getScoopBonus
      | otherwise = getDefaultBonus (last boardP1) (last boardP2)

    summBonus :: Int
    summBonus = topBonus + middleBonus + bottomBonus

    bonusCalculated :: Int
    bonusCalculated = case topBonus + middleBonus + bottomBonus of
      3 -> 6
      (-3) -> -6
      b -> b

foldPoints :: [(Int, Int)] -> Int
foldPoints [] = 0
foldPoints ((r1, r2):xs) = (r1 - r2) + foldPoints xs

middlePoints :: [Int]
middlePoints = [0, 0, 0, 2, 4, 8, 12, 20, 30, 50]

bottomPoints :: [Int]
bottomPoints = map (\p -> if p > 2 then p `div` 2 else 0) middlePoints

pointsCalc :: LineType -> Maybe Combination -> Int
pointsCalc lineType combo = case combo of
  Just c -> getPoints lineType c
  Nothing -> 0

getPoints :: LineType -> Combination -> Int
getPoints Top (RankCombination Pair (Card rank _)) =
    if fromEnum rank < 4 then 0 else fromEnum rank - 3
getPoints Top (RankCombination Set (Card rank _)) = fromEnum rank + 10
getPoints Top _ = 0
getPoints Middle c = middlePoints !! fromEnum (name c)
getPoints Bottom c = bottomPoints !! fromEnum (name c)
