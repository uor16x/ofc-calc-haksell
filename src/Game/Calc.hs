{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use first" #-}
-- | Game calculations and its processing functions
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

-- Type for player input values
data PlayerInput = PlayerInput {
    username :: String,
    board :: [Combination],
    scoop :: Bool,
    withFantasy :: Bool
} deriving (Show, Generic)

instance FromJSON PlayerInput
instance ToJSON PlayerInput


-- Enum for lines types
data LineType = Top | Middle | Bottom deriving (Show, Generic)

instance FromJSON LineType
instance ToJSON LineType


-- Utility type to represent a pair of values
type IntPair = (Int, Int)

{- | Data container for lines comparison result.

First tuple element - list of strings.
Contains usernames of users, which are being compared.

Second tuple element - int.
Represents summ of combination points for first player.
No need to store the value for second user, as it would be the same, just negated (* -1).

Third tuple element - int.
Represents summ of bonuses for first player.
No need to store the value for second user, as it would be the same, just negated (* -1).

Fourth tuple element - list of (int, int) tuples.
This is used mainly for debug and contains each line comparison result.

-}
type LineCompareResult = ([String], Int, Int, [IntPair])


{- | Data container for line result for the user.

Contains data about type of line (position - top/middle/bottom),
about points for the combination,
total values for combo/bonus points after comparison with others.

-}
data LineResult = LineResult {
    lineType :: LineType,
    combination :: Maybe Combination,
    points :: Int,
    totalCombination :: Int,
    totalBonus :: Int
} deriving (Show, Generic)

instance FromJSON LineResult
instance ToJSON LineResult


{- | Container for result of calculations for each player.

Contains data about given player input,
description of each line in form of 'LineResult',
some utility bool values, such as 'isScoop', 'isNextFantasy'.
Also has total sum of points for user after comparison.
And a debug detailed version of the total (totalDetailed field).

-}
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


{- | Main function for game calculation.

Takes a list of 'PlayerInput;, which length should be 3.
Return a list of 'PlayerCalculations'.

First of all, function receives all players comparison using 'collectLinesResults'.
Then it checks for input conditions:
length of input and length of comparison results should be equal and should equal 3.

Then for each of player input function creates 'PlayerCalculations' record.
-}
calcGame :: [PlayerInput] -> [PlayerCalculations]
calcGame playerInputs
  | length playerInputs /= 3 = error "Invalid number of players"
  | length linesResults /= 3 = error "Invalid number of linesResults"
  | otherwise = map mapLinesResults [0..2]
  where
    -- Value which holds result of lines comparison for all inputs
    linesResults :: [(String, [LineCompareResult])]
    linesResults = collectLinesResults [] playerInputs playerInputs

    {-| Mapping function for each given 'PlayerInput'.
        Given int argument - index of player.
    -}
    mapLinesResults :: Int -> PlayerCalculations
    mapLinesResults index = PlayerCalculations {
      -- player is simply copy of input, on which calc is based
      player = playerInputs !! index,

      -- isScoop is bool value to define whether the scoop for the user occurred
      isScoop = scoop $ playerInputs !! index,

      -- isNextFantasy is bool value to determine thether the fantasy
      isNextFantasy =
        not (scoop (playerInputs !! index)) && -- scoop should not occur for fantasy to be true
        nextIsFantasy -- call of function which calculates whether next hand is fantasy
        (withFantasy $ playerInputs !! index) -- withFantasy getter to find out whether fantasy is on currently
        (board $ playerInputs !! index),

      -- top line data
      top = LineResult {
        combination = combinationByIndex (playerInputs !! index) 0, -- top line has index 0,
        lineType = Top,
        points = pointsCalc Top (combinationByIndex (playerInputs !! index) 0),
        totalCombination = fst $ getLinePoints index 0,
        totalBonus = snd $ getLinePoints index 0
      },

      -- middle line data
      middle = LineResult {
        combination = combinationByIndex (playerInputs !! index) 1, -- middle line has index 1,
        lineType = Middle,
        points = pointsCalc Middle (combinationByIndex (playerInputs !! index) 1),
        totalCombination = fst $ getLinePoints index 1,
        totalBonus = snd $ getLinePoints index 1
      },

      -- bottom line data
      bottom = LineResult {
        combination = combinationByIndex (playerInputs !! index) 2, -- bottom line has index 2,
        lineType = Bottom,
        points = pointsCalc Bottom (combinationByIndex (playerInputs !! index) 2),
        totalCombination = fst $ getLinePoints index 2,
        totalBonus = snd $ getLinePoints index 2
      },

      -- debug value with detailed calc steps for total points
      totalDetailed = linesResults !! index,

      -- summ of all related to player totals in one number
      total = totalPoints (linesResults !! index)
      }

    -- simple getter for combination by index of the board
    combinationByIndex :: PlayerInput -> Int -> Maybe Combination
    combinationByIndex playerInput index
      | scoop playerInput = Nothing
      | otherwise = Just $ board playerInput !! index

    -- simpe function to summ combination points of line comparison with its bonus points
    getTotal :: LineCompareResult -> Int
    getTotal (_, combo, bonus, _) = combo + bonus

    -- function to summ all temp results into one total number
    totalPoints ::  (String, [LineCompareResult]) -> Int
    totalPoints (_, compareResults) = foldl (\acc r -> acc + getTotal r) 0 compareResults

    -- function to determine whether the next hand is fantasy
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

    -- getter for points for each exact line compared
    getLinePoints :: Int -> Int -> IntPair
    getLinePoints playerIndex lineIndex
      = foldl
        (\(comboAcc, bonusAcc) (_, _, _, debug) -> (comboAcc + fst (debug !! lineIndex), bonusAcc + snd (debug !! lineIndex)))
        (0, 0)
        (snd (linesResults !! playerIndex))


{-| Function to collect all results of line comparison between all users.

Uses recustion and accumulator to avoid redundant calculations.
For example, if "user1" vs "user2" has been already compared -
no need to compare "user2" vs "user1" - we could just mirror the existing result instead.

Function takes initial acc (empty array by default),
list of player inputs in form of '[PlayerInput]',
and the same list so it could be passed further without mutating it through recursion.

Result is list of three items - one per 'PlayerInput'.
It contains info about username and each line comparison for the user vs other users.

-}
collectLinesResults :: [LineCompareResult] -> [PlayerInput] -> [PlayerInput] -> [(String, [LineCompareResult])]
collectLinesResults acc full (currPlayer:players) =
  (username currPlayer, map (getResult currPlayer) otherInputs) : collectLinesResults (acc ++ map (getResult currPlayer) otherInputs) full players
    where
      -- Filter function to avoid self-comparison
      otherInputs :: [PlayerInput]
      otherInputs = filter (\player -> username player /= username currPlayer) full

      -- Mirroring of comparison results to be able to make ("u1" vs "u2") into ("u2" vs "u1")
      negateTuple :: IntPair -> IntPair
      negateTuple (a, b) = (negate a, negate b)

      -- Function which tries to get existant result from the accumulator to avoid double calc
      getResult :: PlayerInput -> PlayerInput -> ([String], Int, Int, [IntPair])
      getResult player1 player2 = case find (\(usernames, _, _, _) -> usernames == [username player2, username player1] ) acc of
        Just (usernames, combo, bonus, debug) -> (reverse usernames, negate combo, negate bonus, map negateTuple debug)
        _ -> comparePlayers player1 player2
collectLinesResults acc _ [] = []


{-| Function to compare 'PlayerInput' records and retrieve calculated points.
    Takes two 'PlayerInput' as input. Returns calculated points in form of 'LineCompareResult'
 -}
comparePlayers :: PlayerInput -> PlayerInput -> LineCompareResult
comparePlayers
  p1@PlayerInput{ username = p1name, board = boardP1, scoop = p1scoop }
  p2@PlayerInput{ username = p2name, board = boardP2, scoop = p2scoop } = (
    -- List of usernames of compared players
    [p1name, p2name],

    -- Summ of all combo points from all lines compared
    foldPoints [topPoints, middlePoints, bottomPoints],

    -- Sum of all bonuses from all lines compared
    bonusCalculated,

    -- Debug info by each line
    [
      (uncurry (-) topPoints, topBonus),
      (uncurry (-) middlePoints, middleBonus),
      (uncurry (-) bottomPoints, bottomBonus)
    ]
  ) where
    -- Top points getter with scoop considered
    topPoints :: IntPair
    topPoints = (
      if p1scoop then 0 else getPoints Top (head boardP1),
      if p2scoop then 0 else getPoints Top (head boardP2)
      )

    -- Middle points getter with scoop considered
    middlePoints :: IntPair
    middlePoints = (
      if p1scoop then 0 else getPoints Middle (head $ tail boardP1),
      if p2scoop then 0 else getPoints Middle (head $ tail boardP2)
      )

    -- Bottom points getter with scoop considered
    bottomPoints :: IntPair
    bottomPoints = (
      if p1scoop then 0 else getPoints Bottom (last boardP1),
      if p2scoop then 0 else getPoints Bottom (last boardP2)
      )

    -- Create a tuple from number. First elem is the same number, and second one is the number mirrored
    mirrorPoints :: Int -> IntPair
    mirrorPoints p = (p, negate p)

    -- Value which hold info about whether scoop occurred
    anyScoop :: Bool
    anyScoop = p1scoop || p2scoop

    -- Bonus getter for case when no scoop occurred
    getDefaultBonus :: Combination -> Combination -> Int
    getDefaultBonus c1 c2
      | c1 > c2 = 1
      | c1 == c2 = 0
      | otherwise = -1

    -- Bonus getter for case when scoop occurred
    getScoopBonus :: Int
    getScoopBonus = case (p1scoop, p2scoop) of
      (True, True) -> 0
      (True, False) -> -1
      (False, True) -> 1
      (False, False) -> error "getDefaultBonus shouldb be used for this case"

    -- Top line bonus calc with ability for scoop considered
    topBonus :: Int
    topBonus
      | anyScoop = getScoopBonus
      | otherwise = getDefaultBonus (head boardP1) (head boardP2)

     -- Middle line bonus calc with ability for scoop considered
    middleBonus :: Int
    middleBonus
      | anyScoop = getScoopBonus
      | otherwise = getDefaultBonus (head $ tail boardP1) (head $ tail boardP2)

     -- Bottom line bonus calc with ability for scoop considered
    bottomBonus :: Int
    bottomBonus
      | anyScoop = getScoopBonus
      | otherwise = getDefaultBonus (last boardP1) (last boardP2)

    -- Summ of all bonuses calculated
    summBonus :: Int
    summBonus = topBonus + middleBonus + bottomBonus

    -- Extra points for all lines won calculation
    bonusCalculated :: Int
    bonusCalculated = case topBonus + middleBonus + bottomBonus of
      3 -> 6
      (-3) -> -6
      b -> b

-- Function to reduce all points represented as int pairs to a single number
foldPoints :: [IntPair] -> Int
foldPoints [] = 0
foldPoints ((r1, r2):xs) = (r1 - r2) + foldPoints xs

-- Value which holds points data for middle line
middlePoints :: [Int]
middlePoints = [0, 0, 0, 2, 4, 8, 12, 20, 30, 50]

-- Value which holds points data for bottom line, calculated based on 'middlePoints'
bottomPoints :: [Int]
bottomPoints = map (\p -> if p > 2 then p `div` 2 else 0) middlePoints

-- Wrapper for 'getPoints' with ability of combination being 'Nothing' is case of scoop
pointsCalc :: LineType -> Maybe Combination -> Int
pointsCalc lineType combo = case combo of
  Just c -> getPoints lineType c
  Nothing -> 0

-- Combination points getter
getPoints :: LineType -> Combination -> Int
getPoints Top (RankCombination Pair (Card rank _)) =
    if fromEnum rank < 4 then 0 else fromEnum rank - 3
getPoints Top (RankCombination Set (Card rank _)) = fromEnum rank + 10
getPoints Top _ = 0
getPoints Middle c = middlePoints !! fromEnum (name c)
getPoints Bottom c = bottomPoints !! fromEnum (name c)
