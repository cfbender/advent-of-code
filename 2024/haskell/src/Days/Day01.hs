module Days.Day01 (runDay) where

--  Day 1 2024! Spent a lot of time refamiliarizing myself with haskell and getting
--  my tooling fixed. SO much better than day 1 last year tho since it came back to me quickly.
--  Should be good going forward, just gonna take some time to remember things like uncurry again

{- ORMOLU_DISABLE -}
import Data.Bifunctor (bimap)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

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

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map abs . uncurry (zipWith subtract) . bimap sort sort

------------ PART B ------------
partB :: Input -> OutputB
partB (xs, ys) = sum $ zipWith (*) xs similarities
 where
  similarities = map check xs
  check x = Map.findWithDefault 0 x $ U.freqs ys
