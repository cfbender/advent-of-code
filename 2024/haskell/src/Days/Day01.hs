module Days.Day01 where

import Data.Bifunctor (bimap)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Util.Util as U
import Data.Attoparsec.Text (Parser, endOfLine, sepBy, decimal, space, many1)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = unzip <$> line `sepBy` endOfLine
 where
  line = do
    first <- decimal
    many1 space
    second <- decimal
    return (first, second)


------------ TYPES ------------
type Input = ([Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
part1 :: Input -> OutputA
part1 = sum . map abs . uncurry (zipWith subtract) . bimap sort sort

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 (xs, ys) = sum $ zipWith (*) xs similarities
 where
  similarities = map check xs
  check x = Map.findWithDefault 0 x $ U.freqs ys
