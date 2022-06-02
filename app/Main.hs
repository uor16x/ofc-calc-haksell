{-# LANGUAGE DeriveGeneric #-}
-- | Main game module, which handles input processing and the return of result.
module Main where

import OfcCalc ()
import System.Environment ( getArgs )
import Game.Combination ( Combination, parseCombination )
import Game.Board (getUserBoard, Board)
import Data.Either (isLeft)
import CardParts.Cards (Card(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Game.Calc (PlayerInput (..), PlayerCalculations(..))
import Game.Calc ( PlayerInput(PlayerInput), comparePlayers, calcGame )

-- Different steps of input processing before it could be calculated
data Input = InitialInput { username :: String, withFantasy :: Bool, strs :: [String] }
    | BoardInput { username :: String,  withFantasy :: Bool, board :: [[Card]] }
    | CombinationInput { username :: String, scoop :: Bool, withFantasy :: Bool, combinations :: [Combination] } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

main :: IO ()
main = do
    -- read args from command line
    args <- getArgs
    -- check whether length of args is correct (should be 1 arg)
    case length args of
        1 -> case parse (head args) of
                -- if parse succeed - proceed
                Right parseResult -> print $ encode parseResult

                -- otherwise - throw err
                Left msg -> error msg
        _ -> error "Invalid number of arguments"


-- Main processing function to parse input and transform it into a list of results.
parse :: String -> Either String [Game.Calc.PlayerCalculations]
parse userInput = do
    -- transform input so that simple string arg would become 'InitialInput'.
    -- then pass it to 'parseInputCards' to get list of cards instead of string card notations.
    board <- mapM parseInputCards $ parseInput userInput

    -- parse board from given list of cards.
    -- returns parsed board with combinations.
    parsedBoard <- mapM parseBoard board

    -- create 'PlayerInput' value from each board
    playersCalculated <- mapM calcBoard parsedBoard

    -- pass list of 'PlayerInput's to main calc function and return the result
    Right $ calcGame playersCalculated
      where
        -- Transformation function to get 'PlayerInput' from 'Input'
        calcBoard :: Input -> Either String Game.Calc.PlayerInput
        calcBoard inp = Right $
          Game.Calc.PlayerInput 
          (username inp)
          (combinations inp)
          (scoop inp)
          (withFantasy inp)

-- Function to transform string arg into '[Input]'
parseInput :: String -> [Input]
parseInput args =
  map
  (\(name, withFantasy, board) -> InitialInput name withFantasy board)
  (read args :: [(String, Bool, [String])])

-- Function to parse exact cards from 'Input'
parseInputCards :: Input -> Either String Input
parseInputCards InitialInput { username = username,  withFantasy = withFantasy, strs = strs } = case getUserBoard strs of
    Right board -> Right BoardInput {
        username = username,
        withFantasy = withFantasy,
        board = board
    }
    Left msg -> Left $ "Parsing of input cards failed: " ++ msg
parseInputCards _ = Left "Valid InitialInput should be passed here"

-- Function to parse the combinations and form a board from 'Input'
parseBoard :: Input -> Either String Input
parseBoard BoardInput { username = username, withFantasy = withFantasy, board = board } = case parsedCombinations of
    Right result -> Right $ CombinationInput {
        username = username,
        combinations = result,
        withFantasy = withFantasy,
        scoop = isScoop result
    }
    Left msg -> Left $ "Parsing of combinations failed: " ++ msg
    where
        parsedCombinations :: Either String [Combination]
        parsedCombinations
          | length board /= 3 = Right []
          | otherwise = mapM parseCombination board

        isScoop :: [Combination] -> Bool
        isScoop combinations =
            length combinations /= 3 ||
            head combinations > head (tail combinations) ||
            head (tail combinations) > last combinations
parseBoard _ = Left "Valid BoardInput should be passed here"
