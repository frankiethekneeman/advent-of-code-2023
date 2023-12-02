{-|
module: DayOne.PartTwo
description: Advent of Code, Day One, Part Two
-}
module DayOne.PartTwo(Out, solution) where

import Lib.Solution
import Helpers.Solution
import Helpers.Input
import DayOne.PartOne(sumOfCalibrationValues)
import GHC.Data.Maybe(orElse)
import Data.List(stripPrefix)

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("first", 12)
           , ("second", 38)
           , ("third", 15)
           , ("fourth", 77)
           , ("all", 142)
           , ("fifth", 29)
           , ("sixth", 83)
           , ("seventh", 13)
           , ("eighth", 24)
           , ("ninth", 42)
           , ("tenth", 14)
           , ("eleventh", 76)
           , ("second_set", 281)
           , ("overlap", 83)]

prefixes :: [(String, String)]
prefixes = [("one", "1ne")
           , ("two", "2wo")
           , ("three", "3hree")
           , ("four", "4our")
           , ("five", "5ive")
           , ("six", "6ix")
           , ("seven", "7even")
           , ("eight", "8ight")
           , ("nine", "9ine")]
      
digitDecode :: String -> String
digitDecode [] = []
digitDecode garbled = first:digitDecode rest
    where first = head replaced
          rest = tail replaced
          replaced = foldl (flip . uncurry $ replacePrefix) garbled prefixes

replacePrefix :: String -> String -> String -> String
replacePrefix prefix replacement str = replaced `orElse` str
    where replaced = (replacement ++) <$> stripPrefix prefix str

-- | Solution for Day One, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples (always $ lineByLine digitDecode) sumOfCalibrationValues
