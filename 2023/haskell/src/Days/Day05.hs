module Days.Day05 where

-- holy fuckin shit man
-- this was a nightmare for me for some reason
-- of course fell into the noob trap of keeping giant ass lists
-- so I let that run overnight after already staying up too late
--
-- it was wrong.
--
-- so, back to the drawing board. couldn't think through the ranges
-- on my own well enough in haskell so I found some Kotlin solution on
-- reddit that I thought looked nice and tried to implement it here.
--
-- took a bit and learned a lot, but then the answer was wrong. for only
-- my input, even though it got 3 of my coworker's inputs correct.
-- so I guess that guy in Kotlin made some assumption that wasn't true
-- for my input.
--
-- so I ported it to elixir and rewrote the `concatRanges` function there
-- to the ugly guard clause that lives here now.
--
-- and it works. so fuck it. like 15 hours yikes why do people pay me
-- to write software.

{- ORMOLU_DISABLE -}
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
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
concatRanges :: Range Int -> (Range Int, Range Int) -> Maybe [Range Int]
concatRanges input@(inputStart, inputEnd) (src@(srcStart, srcEnd), dest@(destStart, destEnd)) =
  let inRange = inputEnd <= srcEnd && inputEnd >= srcStart
   in if inRange
        then split src dest input
        else Nothing
  where
    shift = destStart - srcStart
    split (ss, se) (ds, de) (is, ie)
      -- input hangs left of source
      | is < ss && ie <= se = Just [(is, ss - 1), (ds, ds + (ie - is))]
      -- input totally in source
      | is >= ss && ie <= se = Just [(is + shift, ie + shift)]
      -- input hangs right of source
      | is >= ss && ie > se = Just [(is + shift, de), (se + 1, ie)]

mapRange :: [(Range Int, Range Int)] -> Range Int -> [Range Int]
mapRange mappings input =
  let output = concat $ mapMaybe (concatRanges input) mappings
   in if null output then [input] else output

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
