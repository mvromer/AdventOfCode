module Day03 (p3a, p3b) where

import Data.List.Split;
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
p3b inputType =
  sum
    . convertToPriorities
    . findBadgeItems
    . TL.lines
    <$> TLIO.readFile (puzzleInput inputType)

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

findBadgeItems :: [TL.Text] -> [Char]
findBadgeItems = map findBadgeItem . chunksOf 3
  where
    findBadgeItem :: [TL.Text] -> Char
    findBadgeItem (x:y:z:[]) =
      let firstSack = Set.fromList (TL.unpack x)
          secondSack = Set.fromList (TL.unpack y)
          thirdSack = Set.fromList (TL.unpack z)
          badgeItem = Set.intersection firstSack . Set.intersection secondSack $ thirdSack
      in Set.elemAt 0 badgeItem

    findBadgeItem xs = error ("Expected three rucksacks for " ++ (TL.unpack . TL.unlines $ xs))

convertToPriorities :: [Char] -> [Int]
convertToPriorities = map (priorityMap Map.!)
  where
    itemTypes = ['a' .. 'z'] ++ ['A' .. 'Z']
    itemPriorities = [1 .. 52]
    priorityMap = Map.fromList (zip itemTypes itemPriorities)

puzzleInput :: PuzzleInput -> String
puzzleInput Example = "./data/day03/example.txt"
puzzleInput Actual = "./data/day03/day03.txt"
