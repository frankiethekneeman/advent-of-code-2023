{-|
module: DayFour.PartTwo
description: Advent of Code, Day Four, Part Two
-}
module DayFour.PartTwo(Out, solution) where

import Lib.Solution
-- import Lib.Types
import Helpers.Solution
import Helpers.Parsing
import Helpers.Input
import DayFour.PartOne(scanCard, Card(..))
import qualified Data.Set as Set

-- import Debug.Trace

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("part1", 30)]

solver :: [Card] -> Out
solver = sum . countCards (repeat 1)

countCards :: [Int] -> [Card] -> [Int]
countCards _ [] = []
countCards [] cards = countCards (repeat 1) cards
countCards (count:counts) ((Card _ winners have):rest) = count:countCards counts' rest
    where counts' = zipWith (+) dupes counts
          dupes = replicate n count ++ repeat 0
          n = length $ Set.intersection winners have

-- | Solution for Day Four, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM $ parse scanCard) (always solver)
