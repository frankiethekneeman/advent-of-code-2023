{-|
module: DayThree.PartTwo
description: Advent of Code, Day Three, Part Two
-}
module DayThree.PartTwo(Out, solution) where

import Lib.Solution
-- import Lib.Types
import Helpers.Solution
import qualified DayThree.PartOne as PartOne
import qualified Data.Set as Set
import Data.Tuple.Extra((&&&))

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("part1", 467835)
           , ("endOfLine", 10)
           ]

solver :: [PartOne.Part] -> [PartOne.Symbol] -> Integer
solver parts symbols = sum gearRatios
    where gearRatios = map (product . map PartOne.partNumber) gears
          gears = filter ((==2) . length) gearCandidates
          gearCandidates = map neighbors stars :: [[PartOne.Part]]
          neighbors symbol = filter (Set.member (getLocation symbol) . PartOne.adjacents) parts
          stars = filter ((=='*') . PartOne.representation) symbols

getLocation :: PartOne.Symbol -> (Int, Int)
getLocation = PartOne.symbolX &&& PartOne.symbolY

-- | Solution for Day Three, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (always PartOne.parser) (always . uncurry $ solver)
