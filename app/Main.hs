{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Main where

import OfcCalc ()
import System.Environment ( getArgs, getProgName )
import Game.Combination ( Combination, parseCombination )
import Game.Board (getUserBoard, Board)
import Data.Either (isLeft)
import CardParts.Cards (Card(..))

data Input = InitialInput { username :: String, strs :: [String] }
    | BoardInput { username :: String, board :: [Card] }
    | CombinationInput { username :: String, combinations :: [Combination] } deriving Show

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case length args of
        2 -> print "Process"
        _ -> error "Invalid number of arguments"
    print $ head args
    print $ args !! 1
    print $ parseInput $ args !! 1

parseInput :: String -> [Input]
parseInput args = map (uncurry InitialInput) (read args :: [(String, [String])])

-- parseInputCards :: InitialInput -> CardsInput
-- parseInputCards = map (\initialInputItem -> (username, getUserBoard cardStrs))

-- parseInputCombinations :: [(String, Either String Board)] -> Either String [(String, [Either String Combination])]
-- parseInputCombinations userBoards
--     | not . null $ failedUserBoards = Left $ show failedUserBoards
--     | otherwise = Right [("str", [])]
--     where
--         failedUserBoards = mapM snd userBoards

-- parseInputCombinations :: [(String, Either String Board)] -> [(String, [Either String Combination])]
-- parseInputCombinations userBoards
--     | not . null $ failedValues = error ""
--     | otherwise = do
--         boards <- userBoards
--         return map (\(u, cds) -> (u, parseCombination $ head cds)) boards
--     where
--         failedValues = filter isLeft $ map snd userBoards