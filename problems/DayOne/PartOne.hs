{-|
module: DayOne.PartOne
description: Advent of Code, Day One, Part One
-}
module DayOne.PartOne(Out, solution, sumOfCalibrationValues) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Distribution.Simple.Utils
import Data.Char

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("first", 12)
           , ("second", 38)
           , ("third", 15)
           , ("fourth", 77)
           , ("all", 142)]

calibrationValue :: String -> Result Int
calibrationValue garbled = maybeToResult "No digits found" number
    where number = read <$> sequence [tens, units]
          tens = safeHead digits
          units = safeLast digits
          digits = filter isDigit garbled

sumOfCalibrationValues :: [String] -> Result Int
sumOfCalibrationValues document = sum <$> values
    where values = sequence $ map calibrationValue document


-- | Solution for Day One, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always lines) sumOfCalibrationValues
