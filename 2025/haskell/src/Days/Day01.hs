module Days.Day01 where

--  Day 1 2025! We back, and it's only 12 days yay!
--  Again struggled to refamiliarize and get my tooling fixed.
--  This one was kinda sneaky hard on part 2 for me, and just ended up brute forcing it.

import Data.Attoparsec.Text (Parser, choice, decimal, endOfLine, sepBy, string)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
 where
  instruction = do
    dir <- choice [string "R" >> return R, string "L" >> return L]
    steps <- decimal
    return (dir, steps)

------------ TYPES ------------
type Input = [(Direction, Int)]

type OutputA = Int

type OutputB = Int

data Direction = R | L
  deriving (Show, Eq)

------------ PART 1 ------------
move :: (Int, Int) -> (Direction, Int) -> (Int, Int)
move (count, curr) (dir, steps) = (count + newCount, newPos)
 where
  newPos = case dir of
    R -> (curr + steps) `mod` 100
    L -> (curr - steps) `mod` 100
  newCount = if newPos == 0 then 1 else 0

part1 :: Input -> OutputA
part1 = fst . foldl move (0, 50)

------------ PART 2 ------------
dial :: Int -> Input -> [Int]
-- recurse through steps to get all positions
dial curr [] = [curr]
dial curr ((dir, steps) : rest) = dial newCurr rest ++ take steps (iterate dial' curr)
 where
  newCurr = case dir of
    R -> (curr + steps) `mod` 100
    L -> (curr - steps) `mod` 100
  dial' curr' = case dir of
    R -> (curr' + 1) `mod` 100
    L -> (curr' - 1) `mod` 100
part2 :: Input -> OutputB
part2 = length . filter (== 0) . dial 50
