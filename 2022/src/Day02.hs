{-# LANGUAGE OverloadedStrings #-}

module Day02 (p2a, p2b) where

import PuzzleInput

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

data Move = Rock | Paper | Scissors
    deriving Show

p2a :: PuzzleInput -> IO Int
p2a inputType = sum . scoreRounds . readRounds . TL.lines <$> TLIO.readFile (puzzleInput inputType)

p2b :: PuzzleInput -> IO Int
p2b inputType = return 0

readRounds :: [TL.Text] -> [(Move, Move)]
readRounds = map parseRound
    where
        parseRound :: TL.Text -> (Move, Move)
        parseRound roundEntry = toRound . (map toMove) . TL.words $ roundEntry

        toMove :: TL.Text -> Move
        toMove move
            | move == "A" || move == "X" = Rock
            | move == "B" || move == "Y" = Paper
            | move == "C" || move == "Z" = Scissors
            | otherwise = error ("Invalid move " ++ (TL.unpack move))

        toRound :: [Move] -> (Move, Move)
        toRound (x:y:[]) = (x,y)
        toRound _ = error "Unexpected number of moves"

scoreRounds :: [(Move, Move)] -> [Int]
scoreRounds = map scoreRound

scoreRound :: (Move, Move) -> Int
scoreRound (Rock, Rock) = rockPoints + tiePoints
scoreRound (Paper, Rock) = rockPoints + losePoints
scoreRound (Scissors, Rock) = rockPoints + winPoints

scoreRound (Rock, Paper) = paperPoints + winPoints
scoreRound (Paper, Paper) = paperPoints + tiePoints
scoreRound (Scissors, Paper) = paperPoints + losePoints

scoreRound (Rock, Scissors) = scissorsPoints + losePoints
scoreRound (Paper, Scissors) = scissorsPoints + winPoints
scoreRound (Scissors, Scissors) = scissorsPoints + tiePoints

rockPoints :: Int
rockPoints = 1

paperPoints :: Int
paperPoints = 2

scissorsPoints :: Int
scissorsPoints = 3

losePoints :: Int
losePoints = 0

tiePoints :: Int
tiePoints = 3

winPoints :: Int
winPoints = 6

puzzleInput :: PuzzleInput -> String
puzzleInput Example = "./data/day02/example.txt"
puzzleInput Actual = "./data/day02/day02.txt"
