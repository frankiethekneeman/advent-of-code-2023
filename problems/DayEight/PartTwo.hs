{-|
module: DayEight.PartTwo
description: Advent of Code, Day Eight, Part Two
-}
module DayEight.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import DayEight.PartOne(parser, Document(..), step)
import qualified Data.Map.Strict as Map
import Data.List(isSuffixOf)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("direct", 2)
           , ("loop", 6)
           , ("ghost", 6) ]

solver :: Document -> Result Out
solver (Document directions network) = length <$> path
    where path = sequence $ takeWhile (not . isFinish) infiniteWalk
          isFinish (Right currs) = all detectEndpoint currs
          isFinish _ = False
          detectEndpoint = isSuffixOf "Z"
          infiniteWalk = scanl next start infiniteDirections
          start = pure . filter (isSuffixOf "A") $ Map.keys network
          next currs dir = sequence . map (step network dir) =<< currs
          infiniteDirections = cycle directions


-- | Solution for Day Eight, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parser solver
