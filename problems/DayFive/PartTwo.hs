{-|
module: DayFive.PartTwo
description: Advent of Code, Day Five, Part Two
-}
module DayFive.PartTwo(Out, solution) where

import Lib.Solution
import Lib.Types
import Helpers.Solution
import DayFive.PartOne(Almanac(..), Lookup(..), LookupMap, parser)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable(foldlM)
import Control.Monad(join)

-- | The type of the answer to this problem
type Out = Integer

examples :: [(String, Out)]
examples = [("partOne", 46)]

combineLookups :: Lookup -> Lookup -> Result Lookup
combineLookups (Lookup lName lForward lBackward) (Lookup rName rForward rBackward) = out
    where out = Lookup name' <$> forward' <*> backward'
          name' = lName ++ rName
          forward' = combineMaps lForward rForward lBackward
          backward'= combineMaps rBackward lBackward rForward

combineMaps :: LookupMap -> LookupMap -> LookupMap -> Result LookupMap
combineMaps first second backward = monadicMapFromSet combineKey =<< keys
    where keys = Set.union firstKeys <$> backPropagatedSecondKeys
          firstKeys = Map.keysSet first
          backPropagatedSecondKeys = monadicSetMap backPropagate $ Map.keysSet second
          backPropagate n = getF n backward <*> Right n
          combineKey k = (.) <$> lhs <*> rhs
            where rhs = join $ getF <$> rhK <*> Right second
                  rhK = lhs <*> Right k
                  lhs = getF k first

monadicSetMap :: (Ord b, Monad m) => (a -> m b) -> Set.Set a -> m (Set.Set b)
monadicSetMap f set =  Set.fromList <$> mapM f (Set.toList set)

monadicMapFromSet :: (Ord a, Monad m) => (a -> m b) -> Set.Set a -> m (Map.Map a b)
monadicMapFromSet f set = Map.fromList <$> pairs
    where pairs = zip keys <$> values
          values = mapM f keys
          keys = Set.toList set


getF :: Integer -> LookupMap -> Result (Integer -> Integer)
getF n lookupMap = maybeToResult errMsg $ snd <$> Map.lookupLE n lookupMap
    where errMsg = "Could not translate " ++ show n ++ " in lookupMap."

baseMap :: LookupMap
baseMap = Map.singleton 0 (+0)

emptyLookup :: Lookup
emptyLookup = Lookup "Empty" baseMap baseMap

solver :: Almanac -> Result Out
solver (Almanac seeds lookups) = minimum <$> candidates
    where candidates = mapM check =<< pairs
          check (start, len) = getMin start (start + len - 1) =<< fullLookup
          pairs = pairsOf seeds
          fullLookup = foldlM combineLookups emptyLookup lookups

pairsOf :: [a] -> Result [(a, a)]
pairsOf (l:r:rest) = ((l,r):) <$> pairsOf rest
pairsOf [] = Right []
pairsOf _ = Left "List has an uneven number of values"

getMin :: Integer -> Integer -> Lookup -> Result Integer
getMin start end (Lookup _ forward _) = minimum <$> candidates
    where candidates = (++) <$> sequence [startFinal, endFinal] <*> restFinal
          startFinal = translate start
          endFinal = translate end
          restFinal = mapM translate $ Map.keys relevantMap
          relevantMap = fst . Map.split end . snd $ Map.split start forward
          translate k = getF k forward <*> Right k

          


-- | Solution for Day Five, Part Two
solution:: AdventProblem Out
solution = adventOfCode examples parser solver
