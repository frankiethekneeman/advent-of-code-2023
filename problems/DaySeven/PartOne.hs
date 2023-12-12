{-|
module: DaySeven.PartOne
description: Advent of Code, Day Seven, Part One
-}
module DaySeven.PartOne(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Helpers.Parsing
import Helpers.Input
import Data.List(sort,group)

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("4k", 5)
           , ("fh", 5)
           , ("Full", 6440)
           , ("OneOfEach", 140)]

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord)

fromChar :: Char -> Result Card
fromChar c = case c of
    '2' -> Right Two
    '3' -> Right Three
    '4' -> Right Four
    '5' -> Right Five
    '6' -> Right Six
    '7' -> Right Seven
    '8' -> Right Eight
    '9' -> Right Nine
    'T' -> Right Ten
    'J' -> Right Jack
    'Q' -> Right Queen
    'K' -> Right King
    'A' -> Right Ace
    other -> Left $ other:" is not a valid card"

instance ReadableFromToken Card where
    readFromChar = fromChar

data Hand = Hand [Card] Integer deriving (Eq)

instance Grokkable Hand where
    fromResult result = Hand <$> cards <*> wager
        where cards = sequence $ map get [0..4] <*> [result]
              wager = get 5 result

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord)

classify :: [Card] -> HandType
classify cards = case reverse . sort . map length . group . sort $ cards of
    (5:_) -> FiveOfAKind
    (4:_) -> FourOfAKind
    (3:2:_) -> FullHouse
    (3:_) -> ThreeOfAKind
    (2:2:_) -> TwoPair
    (2:_) -> OnePair
    _ -> HighCard

parser :: String -> Result [Hand]
parser = lineByLineM (parse handScanner)
    where handScanner = cardsScanner ^& " " ^& scanInt
          cardsScanner = sequential (replicate 5 scanChar)

instance Ord Hand where
    compare (Hand l _) (Hand r _) = case compare (classify l) (classify r) of
        EQ -> compare l r
        other -> other

solver :: [Hand] -> Out
solver = sum . zipWith (*) [1..] . map getWager . sort
    where getWager (Hand _ w) = w


-- | Solution for Day Seven, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parser (always solver)
