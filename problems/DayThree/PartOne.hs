{-|
module: DayThree.PartOne
description: Advent of Code, Day Three, Part One
-}
module DayThree.PartOne(
    Out,
    solution,
    parser,
    Part(..),
    Symbol(..),
    adjacents,

) where

import Lib.Solution
import Helpers.Solution
import Data.Char(isDigit)
import Data.Tuple.Extra((&&&))
import qualified Data.Set as Set

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("part1", 4361)
           , ("endOfLine", 11)
           ]

data Part = Part {
    partNumber :: Integer,
    partX :: Int,
    partY :: Int
} deriving (Show)

data Symbol = Symbol {
    representation :: Char,
    symbolX :: Int,
    symbolY :: Int
} deriving (Show)

getParts :: Int -> Int -> String -> [Part]
getParts _ _ [] = []
getParts row column (char:rest)
    | not . isDigit $ char = getParts row (column + 1) rest
    | otherwise = part : getParts row (column + len) rest'
        where part = Part num row column
              len = 1 + length digits
              num = read (char:digits)
              (digits, rest') = span isDigit rest

getSymbols :: Int -> Int -> String -> [Symbol]
getSymbols _ _ [] = []
getSymbols row column (char:rest)
    | isDigit char || char == '.' = getSymbols row (column + 1) rest
    | otherwise = symbol : getSymbols row (column + 1) rest
        where symbol = Symbol char row column

scanFor :: (Int -> Int -> String -> [a]) -> String -> [a]
scanFor scanner input = scan =<< zip [0..] (lines input)
    where scan (rowNo, row) = scanner rowNo 0 row

parser :: String -> ([Part], [Symbol])
parser = scanFor getParts &&& scanFor getSymbols

mkSet :: [Symbol] -> Set.Set (Int, Int)
mkSet = Set.fromList . map (symbolX &&& symbolY)

adjacents :: Part -> Set.Set (Int, Int)
adjacents part = Set.fromList (above ++ below ++ [left, right])
    where above = [ (x + 1, y) | y <- [yMin..yMax]]
          below = [ (x - 1, y) | y <- [yMin..yMax]]
          left = (x, yMin)
          right = (x, yMax)
          x = partX part
          yMax = yMin + numLength 10 (partNumber part) + 1
          yMin = partY part - 1

numLength :: Integral a => a -> a -> Int
numLength base n
    | n < 0 = 1 + numLength base (-n)
    | n >= base = 1 + numLength base (n `quot` base)
    | otherwise = 1


solver :: [Part] -> [Symbol] -> Out
solver parts symbols = sum . map partNumber $ relevant
    where relevant = filter isAdjacent parts
          isAdjacent part = not $ Set.disjoint symbolLocations (adjacents part)
          symbolLocations = mkSet symbols

-- | Solution for Day Three, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (always parser) (always . uncurry $ solver)
