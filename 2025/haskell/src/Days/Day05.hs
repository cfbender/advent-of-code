module Days.Day05 where

-- Data.Range coming in huge here, took me a while to get the bounds before I figured out
-- I could just pattern match it out like I did here.
-- Made it super easy, though I think the joinRanges wouldn't have been that hard to write otherwise

import Data.Attoparsec.Text (Parser, char, count, decimal, endOfLine, sepBy)
import Data.Range (Bound (..), BoundType (..), Range (SpanRange), (+=+))
import Data.Range qualified as Range
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    ranges <- range `sepBy` endOfLine
    count 2 endOfLine
    ingredients <- decimal `sepBy` endOfLine
    return (ranges, ingredients)
  where
    range = do
        start <- decimal
        char '-'
        end <- decimal
        return (start +=+ end)

------------ TYPES ------------
type Input = ([Range Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
-- 389 too low
part1 :: Input -> OutputA
part1 (ranges, ingredients) = length [i | i <- ingredients, Range.inRanges allRanges i]
  where
    allRanges = Range.mergeRanges ranges

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 (ranges, _) = sum $ map (\(x, y) -> y - x + 1) allRanges
  where
    -- merge all ranges to knock out overlaps
    allRanges = map toBounds $ Range.joinRanges ranges
    toBounds :: Range Int -> (Int, Int)
    toBounds (SpanRange (Bound x Inclusive) (Bound y Inclusive)) = (x, y)
