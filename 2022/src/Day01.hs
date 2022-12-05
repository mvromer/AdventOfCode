{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Day01 (p1a, p1b) where

import Data.List
import PuzzleInput
import qualified Data.ByteString.Lazy.Char8 as BS

p1a :: PuzzleInput -> IO ()
p1a inputType = do
    puzzle <- BS.readFile (puzzleInput inputType)
    let maxCalories = maximum . readCalories . BS.lines $ puzzle
    putStrLn (show maxCalories)

p1b :: PuzzleInput -> IO ()
p1b inputType = do
    puzzle <- BS.readFile (puzzleInput inputType)
    let top3Calories = sum . take 3 . reverse . sort . readCalories . BS.lines $ puzzle
    putStrLn (show top3Calories)

readCalories :: [BS.ByteString] -> [Int]
readCalories puzzle = lastElfCalories:caloriesPerElf
    where
        (caloriesPerElf, lastElfCalories) = foldl (updateCalories) ([], 0) puzzle

        updateCalories :: ([Int], Int) -> BS.ByteString -> ([Int], Int)
        updateCalories (currentCaloriesPerElf, currentElfCalories) EmptyByteString =
            (currentElfCalories:currentCaloriesPerElf, 0)

        updateCalories (currentCaloriesPerElf, currentElfCalories) snackCalories =
            case BS.readInt snackCalories of
                Just (calories, _) -> (currentCaloriesPerElf, calories + currentElfCalories)
                _ -> error ("Unexpected calorie" ++ (BS.unpack snackCalories))

pattern EmptyByteString :: BS.ByteString
pattern EmptyByteString <- (BS.uncons -> Nothing)

puzzleInput :: PuzzleInput -> String
puzzleInput Example = "./data/day01/example.txt"
puzzleInput Actual = "./data/day01/day01.txt"
