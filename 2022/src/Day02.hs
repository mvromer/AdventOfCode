{-# LANGUAGE OverloadedStrings #-}

module Day02 (p2a, p2b) where

import PuzzleInput

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

data Move = Rock | Paper | Scissors
  deriving Show

data Outcome = Win | Lose | Draw
  deriving Show

p2a :: PuzzleInput -> IO Int
p2a inputType =
  sum
    . scoreRounds
    . readRounds
    . TL.lines
    <$> TLIO.readFile (puzzleInput inputType)

p2b :: PuzzleInput -> IO Int
p2b inputType =
  sum
    . scoreRounds
    . convertToRounds
    . readStrategy
    . TL.lines
    <$> TLIO.readFile (puzzleInput inputType)

readRounds :: [TL.Text] -> [(Move, Move)]
readRounds = map parseRound
  where
    parseRound :: TL.Text -> (Move, Move)
    parseRound entry = toRound . TL.words $ entry

    toRound :: [TL.Text] -> (Move, Move)
    toRound (x:y:[]) = (toMove x, toMove y)
    toRound x = error ("Unexpected number of moves in " ++ (TL.unpack . TL.unwords $ x))

    toMove x
      | x == "A" || x == "X" = Rock
      | x == "B" || x == "Y" = Paper
      | x == "C" || x == "Z" = Scissors
      | otherwise = error ("Invalid move " ++ (TL.unpack x))

readStrategy :: [TL.Text] -> [(Move, Outcome)]
readStrategy = map parseStrategy
  where
    parseStrategy :: TL.Text -> (Move, Outcome)
    parseStrategy entry = toStrategy . TL.words $ entry

    toStrategy :: [TL.Text] -> (Move, Outcome)
    toStrategy (x:y:[]) = (toMove x, toOutcome y)
    toStrategy x = error ("Unexpected number of entries in " ++ (TL.unpack . TL.unwords $ x))

    toMove x
      | x == "A" = Rock
      | x == "B" = Paper
      | x == "C" = Scissors
      | otherwise = error ("Unexpected move " ++ (TL.unpack x))

    toOutcome x
      | x == "X" = Lose
      | x == "Y" = Draw
      | x == "Z" = Win
      | otherwise = error ("Unexpected outcome " ++ (TL.unpack x))

convertToRounds :: [(Move, Outcome)] -> [(Move, Move)]
convertToRounds = map convertToRound
  where
    convertToRound (move, Draw) = (move, move)

    convertToRound (Rock, Win) = (Rock, Paper)
    convertToRound (Rock, Lose) = (Rock, Scissors)

    convertToRound (Paper, Win) = (Paper, Scissors)
    convertToRound (Paper, Lose) = (Paper, Rock)

    convertToRound (Scissors, Win) = (Scissors, Rock)
    convertToRound (Scissors, Lose) = (Scissors, Paper)

scoreRounds :: [(Move, Move)] -> [Int]
scoreRounds = map scoreRound
  where
    scoreRound (Rock, Rock) = rockPoints + tiePoints
    scoreRound (Paper, Rock) = rockPoints + losePoints
    scoreRound (Scissors, Rock) = rockPoints + winPoints

    scoreRound (Rock, Paper) = paperPoints + winPoints
    scoreRound (Paper, Paper) = paperPoints + tiePoints
    scoreRound (Scissors, Paper) = paperPoints + losePoints

    scoreRound (Rock, Scissors) = scissorsPoints + losePoints
    scoreRound (Paper, Scissors) = scissorsPoints + winPoints
    scoreRound (Scissors, Scissors) = scissorsPoints + tiePoints

    rockPoints = 1
    paperPoints = 2
    scissorsPoints = 3
    losePoints = 0
    tiePoints = 3
    winPoints = 6

puzzleInput :: PuzzleInput -> String
puzzleInput Example = "./data/day02/example.txt"
puzzleInput Actual = "./data/day02/day02.txt"
