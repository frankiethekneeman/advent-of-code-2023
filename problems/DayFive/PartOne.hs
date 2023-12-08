{-|
module: DayFive.PartOne
description: Advent of Code, Day Five, Part One
-}
module DayFive.PartOne(Out, solution, Almanac(..), Lookup(..), LookupMap, parser) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import Helpers.Parsing
import qualified Data.Map.Strict as Map
import GHC.Data.Maybe

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [ ("seed79", 82)
           , ("seed14", 43)
           , ("seed55", 86)
           , ("seed13", 35)
           , ("partOne", 35) ]

type LookupMap = Map.Map Integer (Integer -> Integer)

data Almanac = Almanac [Integer] [Lookup] deriving Show
data LookupEntry = LookupEntry Integer Integer Integer 
data Lookup = Lookup String LookupMap LookupMap

instance Show Lookup where
    show (Lookup name forward backward) = name ++ " (" ++ printEntries forward  ++ ")(" ++ printEntries backward ++")"
        where printEntries = show . map printEntry . Map.toList 
              printEntry (start, f) = show start ++ "-> +" ++ (show . f $ 0)

instance Grokkable Almanac where
    fromResult = grok2 Almanac 

instance Grokkable Lookup where
    fromResult result = Lookup <$> name <*> forward <*> backward
        where name = get 0 result
              forward = toLookup <$> lookups
              backward = toLookup . map invert <$> lookups
              lookups = get 1 result

instance Grokkable LookupEntry where
    fromResult = grok3 LookupEntry

baseLookup :: Map.Map Integer (Maybe Integer)
baseLookup = Map.singleton 0 Nothing 

invert :: LookupEntry -> LookupEntry
invert (LookupEntry dest source len) = LookupEntry source dest len


toLookup :: [LookupEntry] -> LookupMap
toLookup = Map.map ((+) . flip orElse 0) . Map.unionsWith firstJust . (baseLookup:) . map mkLookup
    where mkLookup (LookupEntry dest source len) = Map.fromList [(source, Just $ dest - source), (source + len, Nothing)]

almanacScanner :: Scanner
almanacScanner = seeds ^& "\n\n" ^& (mapScanner ^* "\n")
    where seeds = "seeds: " ^& (scanInt ^* " ")
          mapScanner = mapName ^& " map:\n" ^& (triple ^* "\n")
          triple = scanInt ^& " " ^& scanInt ^& " " ^& scanInt
          mapName = alternating $ map remember [ "seed-to-soil"
                                               , "soil-to-fertilizer"
                                               , "fertilizer-to-water"
                                               , "water-to-light"
                                               , "light-to-temperature"
                                               , "temperature-to-humidity"
                                               , "humidity-to-location" ]

parser :: String -> Result Almanac
parser = parse almanacScanner

solver :: Almanac -> Result Out
solver (Almanac seeds lookups) = minimum . map doLookups $ seeds
    where doLookups seed = foldl transition (Right seed) lookups

transition :: Result Integer -> Lookup -> Result Integer
transition (Left msg) _ = Left msg
transition (Right num) (Lookup name forward _) = f <*> Right num
    where f = snd <$> pair
          pair = maybeToResult (show num ++ " not in " ++ name) $ Map.lookupLE num forward
          

-- | Solution for Day Five, Part One
solution:: AdventProblem Out
solution = adventOfCode examples parser solver
