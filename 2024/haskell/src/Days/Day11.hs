module Days.Day11 where

--- UHGGGGH IT'S TOO LATE
--
-- I saw this problem and thought it looked really fun. Did part 1 super fast and then realized
-- it was one of those "you can't do the naive solution" ones. Should've probably given up
-- but didn't for some reason. Closed my laptop and then remembered keeping counts instead in previous years.
-- Really thought it was gonna be some sort of memoization but really it was just a data structure problem
-- and I should've seen that faster.
--
-- Ended up pretty happy with a cute solution tho so that's cool

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Program.RunDay qualified as R (Day, runDay)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ' '

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

change :: Int -> Int -> Map Int Int -> Map Int Int
change x y stones
  | x == 0 = M.alter add 1 stones
  | even $ length digits =
      let (a, b) = splitHalf digits
          stones' = M.alter add (U.undigits a) stones
       in M.alter add (U.undigits b) stones'
  | otherwise = M.alter add (x * 2024) stones
 where
  digits = U.digits x
  -- add the count of new stones to the previous based on how many came in
  add (Just x) = Just (x + y)
  add Nothing = Just y

blink :: Int -> [Int] -> Int
blink n input = M.foldr (+) 0 $ foldr simulate stones [1 .. n]
 where
  stones = U.freq input
  -- go over each stone and change it passing along the current number
  simulate _ = M.foldrWithKey change M.empty

part1 :: Input -> OutputA
part1 = blink 25

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = blink 75
