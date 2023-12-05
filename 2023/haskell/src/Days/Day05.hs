module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Debug.Trace
import Data.Bifunctor
import Control.Monad
import Data.List
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Util.Parsers
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser Input
inputParser = do
  string "seeds:"
  space
  seeds <- decimal `sepBy` space
  count 2 endOfLine
  maps <- category `sepBy` count 2 endOfLine
  return
    (seeds, reverse maps)
  where
    word = many1 letter
    category = do
      word `sepBy` "-"
      space
      word
      string ":"
      endOfLine
      mapLine `sepBy` endOfLine
    mapLine = do
      dest <- decimal
      space
      source <- decimal
      space
      range <- decimal
      return ((source, source + range - 1), (dest, dest + range - 1))

------------ TYPES ------------
type Range a = (a, a)

type Almanac = [[(Range Int, Range Int)]]

type Input = ([Int], Almanac)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
defaultEmpty :: [a] -> [a] -> [a]
defaultEmpty d [] = d
defaultEmpty _d l = l

concatRanges :: Range Int -> (Range Int, Range Int) -> Maybe (Range Int)
concatRanges input (src, dest) =
  let shift = fst dest - fst src
      start = max (fst src) (fst input)
      end = min (snd src) (snd input)
   in if end < start then Nothing else Just (start + shift, end + shift)

-- find values in the input that weren't covered by the mappings
passUnmapped :: Range Int -> [(Range Int, Range Int)] -> [Range Int]
passUnmapped (start, end) mappings =
  let theirMin = minimum $ map (fst . fst) mappings
      theirMax = maximum $ map (snd . fst) mappings
      newMins = ([(start, theirMin - 1) | start < theirMin])
      newMaxs = ([(theirMax + 1, end) | end > theirMax])
   in newMins ++ newMaxs

mapRange :: [(Range Int, Range Int)] -> Range Int -> [Range Int]
mapRange mappings input =
  let outputs = mapMaybe (concatRanges input) mappings
      rest = if null outputs then [input] else passUnmapped input mappings
   in outputs ++ rest

iterateSeeds :: [Range Int] -> Almanac -> Int
iterateSeeds seeds maps =
  let results = concatMap (\seed -> foldr folder [seed] maps) seeds
   in minimum $ map fst results
  where
    folder rangeMappings = concatMap (mapRange rangeMappings)

partA :: Input -> OutputA
partA (seeds, maps) = iterateSeeds seedRanges maps
  where
    seedRanges = map (\i -> (i, i)) seeds

------------ PART B ------------
partB :: Input -> OutputB
partB (seeds, maps) = iterateSeeds seedRanges maps
  where
    seedRanges = map (\[i, x] -> (i, i + x - 1)) (chunksOf 2 seeds)
