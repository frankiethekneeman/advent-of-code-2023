{-|
module: DayTwo.PartTwo
description: Advent of Code, Day Two, Part Two
-}
module DayTwo.PartTwo(Out, solution) where

import Lib.Solution
-- import Lib.Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Helpers.Solution
import DayTwo.PartOne(parser, Color(..), Game(..), Cubes)

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("one", 48)
           , ("two", 12)
           , ("three", 1560)
           , ("four", 630)
           , ("five", 36)
           , ("oneThruFive", 2286)]

minOf :: Game -> Color -> Integer
minOf (Game _ reveals) c = maximum counts
    where counts = map (Map.findWithDefault 0 c) reveals

colors :: Set.Set Color
colors = Set.fromList [Red, Blue, Green]

minCubes :: Game -> Cubes
minCubes g = Map.fromSet (minOf g) colors

power :: Game -> Integer
power = product . minCubes

combinedGamePowers :: [Game] -> Integer
combinedGamePowers = sum . map power

-- | Solution for Day Two, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parser (always combinedGamePowers)

