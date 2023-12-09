{-|
module: DaySix.PartTwo
description: Advent of Code, Day Six, Part Two
-}
module DaySix.PartTwo(Out, solution) where

import Lib.Solution
--import Lib.Types
import Helpers.Solution
import DaySix.PartOne(parser, solver)

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [("allRaces", 71503)]

-- | Solution for Day Six, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (parser . filter (/=' ')) (always solver)
