{-|
module: DayFour.PartOne
description: Advent of Code, Day Four, Part One
-}
module DayFour.PartOne(Out, solution, scanCard, Card(..)) where

import Lib.Solution
-- import Lib.Types
import Helpers.Solution
import qualified Data.Set as Set
import Helpers.Parsing
import Helpers.Input
import Data.Bits

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [("part1", 13)]

data Card = Card Integer (Set.Set Integer) (Set.Set Integer)

fromLists :: Integer -> [Integer] -> [Integer] -> Card
fromLists n winners have = Card n (Set.fromList winners) (Set.fromList have)

instance Grokkable Card where
    fromResult = grok3 fromLists

scanCard :: Scanner
scanCard = "Card" ^& space ^& scanInt ^& ":" ^& space ^& numbers ^& "|" ^& space ^& numbers
    where numbers = scanInt ^* space
          space = "   " ^| "  " ^| " "

solver :: [Card] -> Out
solver = sum . map score

score :: Card -> Int
score (Card _ winning have) = shiftR (shiftL 1 matches) 1
    where matches = length (Set.intersection winning have) 


-- | Solution for Day Four, Part One
solution:: AdventProblem Out
solution = adventOfCode examples (lineByLineM $ parse scanCard) (always solver)
