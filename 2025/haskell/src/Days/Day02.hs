module Days.Day02 where

-- day 2! Got tripped up on part 2, and ended up rewriting it so many times until this much simpler solution.
-- classic case of overcomplicating it really.

import Data.Attoparsec.Text (Parser, char, decimal, sepBy)
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = range `sepBy` char ','
 where
  range = do
    start <- decimal
    _ <- char '-'
    end <- decimal
    return (start, end)

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
-- Check if a number is made of two identical halves
invalid :: Int -> Bool
invalid num =
  let s = show num
      n = length s
   in even n && take (n `div` 2) s == drop (n `div` 2) s

part1 :: Input -> OutputA
part1 = sum . concatMap filterInvalid
 where
  filterInvalid :: (Int, Int) -> [Int]
  filterInvalid (start, end) = [n | n <- [start .. end], invalid n]

------------ PART 2 ------------
-- Check if a number is made of a pattern repeated at least twice
invalid2 :: Int -> Bool
invalid2 num = any isRepeatingPattern [1 .. length s `div` 2]
 where
  s = show num
  isRepeatingPattern patternLen =
    length s `mod` patternLen == 0
      && let pattern = take patternLen s
             numRepeats = length s `div` patternLen
          in numRepeats >= 2 && concat (replicate numRepeats pattern) == s

part2 :: Input -> OutputB
part2 = sum . concatMap filterInvalid2
 where
  filterInvalid2 :: (Int, Int) -> [Int]
  filterInvalid2 (start, end) = [n | n <- [start .. end], invalid2 n]
