module Days.Day03 where

-- a nice fun one, spent too long on part 3 trying to be cute with sorting the digits
-- but that didn't work out. then I realized it's just recursively picking the max in the first n
-- digits where n is how many we have minus 12 (or however many have already been picked) since the earliest digits are the most important
--
-- that worked great first try once I wrote it!
import Data.Attoparsec.Text (Parser, digit, endOfLine, many1, sepBy)
import Data.Char (digitToInt)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Vector (fromList, indexed, init, maximumBy)
import Data.Vector qualified as V
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Util (undigits, windows)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = bank `sepBy` endOfLine
  where
    bank = do
        n <- many1 digit
        return (map digitToInt n)

------------ TYPES -----.-------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
part1 :: Input -> OutputA
part1 = sum . map result
  where
    result xs =
        let v = fromList xs
            (firstIdx, first) = maximumBy (comparing snd) $ V.init $ indexed v
            second = maximum $ V.drop (firstIdx + 1) v
         in undigits [first, second]

------------ PART 2 ------------
fill :: Int -> V.Vector (Int, Int) -> [Int] -> [Int]
fill x v l
    | x == 0 = l -- return when no more needed
    | otherwise =
        let
            -- take length - x values from start
            window = V.take (V.length v - x + 1) v
            -- find max
            (idx, val) = maximumBy (comparing snd) window
         in
            -- fill with rest and x - 1
            fill (x - 1) (V.filter (\(i, _) -> i > idx) v) (val : l)

part2 :: Input -> OutputB
part2 = sum . map (undigits . result)
  where
    result xs =
        let v = indexed $ fromList xs
         in reverse $ fill 12 v []
