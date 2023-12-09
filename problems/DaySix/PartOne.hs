{-|
module: DaySix.PartOne
description: Advent of Code, Day Six, Part One
-}
module DaySix.PartOne(Out, solution, parser, solver) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Helpers.Parsing

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("firstrace", 4)
           , ("secondrace", 8)
           , ("thirdrace", 9)
           , ("allRaces", 288) ]

data Race = Race Integer Integer

instance Grokkable [Race] where
    fromResult result = zipWith Race <$> times <*> distances
        where times = get 0 result
              distances = get 1 result

racesScanner :: Scanner
racesScanner = "Time:" ^& intList ^& "\nDistance:" ^& intList
    where intList = spaces ^& (scanInt ^* spaces)
          spaces = discard (==' ')

parser :: String -> Result [Race]
parser = parse racesScanner

solver :: [Race] -> Out
solver = product . map calcWinning
    where calcWinning (Race time dist) = time + 1 - (2 * losing)
            where losing = toInteger . length . takeWhile (<=dist) $ zipWith (*) speeds remainings
                  remainings = reverse speeds
                  speeds = [0..time]

-- | Solution for Day Six, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parser (always solver)
