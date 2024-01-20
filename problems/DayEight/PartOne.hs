{-|
module: DayEight.PartOne
description: Advent of Code, Day Eight, Part One
-}
module DayEight.PartOne(Out, solution, parser, Network, Document(..), step) where

import Lib.Solution
import Lib.Types
import Helpers.Parsing
import qualified Data.Map.Strict as Map

-- | The type of the answer to this problem
type Out = Int

examples :: [(String, Out)]
examples = [ ("direct", 2)
           , ("loop", 6)]

type Network = Map.Map String (String, String)
data Node = Node String String String
data Direction = R | L
data Document = Document [Direction] Network

instance ReadableFromToken [Direction] where
    readFromString = mapM charToDirection 
        where charToDirection 'R' = Right R
              charToDirection 'L' = Right L
              charToDirection c = Left $ "Unexpected direction: " ++ show c

instance Grokkable Document where
    fromResult result = Document <$> directions <*> network
        where directions = get 0 result
              network = Map.fromList . map asTuple <$> get 1 result
              asTuple (Node key left right) = (key, (left, right))

instance Grokkable Node where
    fromResult result = Node <$> key <*> left <*> right
        where key = get 0 result
              left = get 1 result
              right = get 2 result

parser :: String -> Result Document
parser = parse $ directionScanner ^& "\n\n" ^& (nodeScanner ^* "\n") ^& ()
    where directionScanner = (/='\n')
          nodeScanner = chomp 3 ^& " = (" ^& chomp 3 ^& ", " ^& chomp 3 ^& ")"

solver :: Document -> Result Int
solver (Document directions network) = length <$> path
    where path = sequence $ takeWhile (/=Right "ZZZ") infiniteWalk
          infiniteWalk = scanl next (Right "AAA") infiniteDirections
          infiniteDirections = cycle directions 
          next curr dir = step network dir =<< curr

step :: Network -> Direction -> String -> Result String
step network dir name = case (dir, Map.lookup name network) of
    (R, Just (_, name')) -> Right name'
    (L, Just (name', _)) -> Right name'
    (_, Nothing) -> Left $ "Unrecognized node: " ++ name

-- | Solution for Day Eight, Part One
solution :: AdventProblem Out
solution = adventOfCode examples parser solver
