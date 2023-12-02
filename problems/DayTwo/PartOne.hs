{-|
module: DayTwo.PartOne
description: Advent of Code, Day Two, Part One
-}
module DayTwo.PartOne(Out, solution, Game(..), Cubes, Color(..), parser) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Helpers.Parsing
import Helpers.Input
import qualified Data.Map.Strict as Map


-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("one", 1)
           , ("two", 2)
           , ("three", 0)
           , ("four", 0)
           , ("five", 5)
           , ("oneThruFive", 8)]

data Color = Red | Green | Blue deriving (Eq, Ord)
type Cubes = Map.Map Color Integer
data Game = Game Integer [Cubes]

getId :: Game -> Integer
getId (Game n _) = n

fromStr :: String -> Result Color
fromStr "red" = Right Red
fromStr "blue" = Right Blue
fromStr "green" = Right Green
fromStr s = Left $ "Expected known color but got " ++ s

instance ReadableFromToken Color where
    readTok (StrTok s) = fromStr s
    readTok _ = Left "Can only parse color from string"

instance Grokkable (Color, Integer) where
    fromResult = grok2 $ flip (,)

instance Grokkable Cubes where
    fromResult r = Map.fromList <$> get 0 r

instance Grokkable Game where
    fromResult = grok2 Game

scanGame :: Scanner
scanGame = "Game " ^& scanInt ^& ": " ^& (scanReveal ^* "; ") ^& ()
    where scanReveal = scanCube ^* ", "
          scanCube = scanInt ^& " " ^& scanColor
          scanColor = alternating $ map remember ["red", "blue", "green"]

isPossible :: Cubes -> Cubes -> Bool
isPossible setup candidate = all sufficient [Red, Blue, Green]
    where sufficient c = Map.findWithDefault 0 c setup >= Map.findWithDefault 0 c candidate

isLegalGame :: Cubes -> Game -> Bool
isLegalGame setup (Game _ reveals) = all (isPossible setup) reveals

baseSetup :: Cubes
baseSetup = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

sumLegalGames :: [Game] -> Integer
sumLegalGames = sum . map getId . filter (isLegalGame baseSetup)

parser :: String -> Result [Game]
parser = lineByLineM $ parse scanGame

-- | Solution for Day Two, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parser (always sumLegalGames)
