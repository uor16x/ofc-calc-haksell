{-# LANGUAGE DeriveGeneric #-}
module Main where

import OfcCalc ()
import System.Environment ( getArgs, getProgName )
import Game.Combination ( Combination, parseCombination )
import Game.Board (getUserBoard, Board)
import Data.Either (isLeft)
import CardParts.Cards (Card(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Game.Calc (PlayerInput (..), PlayerCalculations(..))
import Game.Calc ( PlayerInput(PlayerInput), comparePlayers, calcGame )

data Input = InitialInput { username :: String, strs :: [String] }
    | BoardInput { username :: String, board :: [[Card]] }
    | CombinationInput { username :: String, scoop :: Bool, combinations :: [Combination] } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case length args of
        1 -> case parse (head args) of
                Right parseResult -> print $ encode parseResult
                Left msg -> error msg
        _ -> error "Invalid number of arguments"

-- parse :: String -> Either String [Game.Calc.PlayerCalculations]
parse userInput = do
    board <- mapM parseInputCards $ parseInput userInput
    parsedBoard <- mapM parseBoard board
    playersCalculated <- mapM calcBoard parsedBoard
    Right $ calcGame playersCalculated
    -- Right [
    --   comparePlayers (head playersCalculated) (head $ tail playersCalculated),
    --   comparePlayers (head $ tail playersCalculated) (last playersCalculated),
    --   comparePlayers (last playersCalculated) (head playersCalculated)
    --   ]
    -- Right $ calcGame [] playersCalculated
    -- Right $ calcUsers $ calcSteps playersCalculated
      where
        calcBoard :: Input -> Either String Game.Calc.PlayerInput
        calcBoard inp = Right $ Game.Calc.PlayerInput (username inp) (combinations inp) (scoop inp)

        -- calcSteps :: [Game.Calc.PlayerCalculations] -> [(Game.Calc.PlayerCalculations, Game.Calc.PlayerCalculations)]
        -- calcSteps (p1:p2:p3:_) = [
        --     comparePairOfPlayers(p1, p2),
        --     comparePairOfPlayers(p2, p3),
        --     comparePairOfPlayers(p3, p1)
        --   ]
        -- calcSteps _ = error "Wrong size of calcSteps input array"

        -- calcUsers :: [(Game.Calc.PlayerCalculations, Game.Calc.PlayerCalculations)] -> [Game.Calc.PlayerCalculations]
        -- calcUsers ((p1r1, p2r1) : (p2r2, p3r1) : (p3r2, p1r2) : _) = [
        --   updateTotals p1r1 p1r2,
        --   updateTotals p2r1 p2r2,
        --   updateTotals p3r1 p3r2
        --  ]
        -- calcUsers _ = error "Wrong size of calcUsers input array"

        -- firstUserResult p1 p2 p3 = comparePairOfPlayers (fst $ comparePairOfPlayers (p1, p2), p3)
        -- secondUserResult p1 p2 p3 = comparePairOfPlayers (snd $ comparePairOfPlayers (p1, p2), snd $ firstUserResult p1 p2 p3)
        -- thirdUserResult p1 p2 p3 = comparePairOfPlayers (fst $ firstUserResult p1 p2 p3, snd $ secondUserResult p1 p2 p3)


parseInput :: String -> [Input]
parseInput args = map (uncurry InitialInput) (read args :: [(String, [String])])

parseInputCards :: Input -> Either String Input
parseInputCards InitialInput { username = username, strs = strs } = case getUserBoard strs of
    Right board -> Right BoardInput {
        username = username,
        board = board
    }
    Left msg -> Left $ "Parsing of input cards failed: " ++ msg
parseInputCards _ = Left "Valid InitialInput should be passed here"

parseBoard :: Input -> Either String Input
parseBoard BoardInput { username = username, board = board } = case parsedCombinations of
    Right result -> Right $ CombinationInput {
        username = username,
        combinations = result,
        scoop = isScoop result
    }
    Left msg -> Left $ "Parsing of combinations failed: " ++ msg
    where
        parsedCombinations = mapM parseCombination board
        isScoop combinations =
            head combinations > head (tail combinations)
            || head (tail combinations) > last combinations
parseBoard _ = Left "Valid BoardInput should be passed here"
