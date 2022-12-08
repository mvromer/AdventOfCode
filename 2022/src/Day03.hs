module Day03 (p3a, p3b) where

import PuzzleInput

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

p3a :: PuzzleInput -> IO Int
p3a inputType =
  sum
    . convertToPriorities
    . findOutOfPlaceItems
    . TL.lines
    <$> TLIO.readFile (puzzleInput inputType)

p3b :: PuzzleInput -> IO Int
p3b inputType = return 0

findOutOfPlaceItems :: [TL.Text] -> [Char]
findOutOfPlaceItems = map findOutOfPlaceItem
  where
    findOutOfPlaceItem :: TL.Text -> Char
    findOutOfPlaceItem rucksack =
      let numberItems = TL.length rucksack
          (firstCompartment, secondCompartment) = TL.splitAt (numberItems `div` 2) rucksack
          firstItemTypes = Set.fromList (TL.unpack firstCompartment)
          secondItemTypes = Set.fromList (TL.unpack secondCompartment)
      in Set.elemAt 0 (Set.intersection firstItemTypes secondItemTypes)

convertToPriorities :: [Char] -> [Int]
convertToPriorities = map (priorityMap Map.!)
  where
    itemTypes = ['a' .. 'z'] ++ ['A' .. 'Z']
    itemPriorities = [1 .. 52]
    priorityMap = Map.fromList (zip itemTypes itemPriorities)

puzzleInput :: PuzzleInput -> String
puzzleInput Example = "./data/day03/example.txt"
puzzleInput Actual = "./data/day03/day03.txt"
