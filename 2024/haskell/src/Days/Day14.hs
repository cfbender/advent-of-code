module Days.Day14 where

-- this was such a weird one
-- reminds me of one in... 2020? where you had to print
-- out to do something human readable entering the characters
--
-- part 1 was easy and fun, glad the modulo works so easily
--
-- I simply have ZERO idea how to print in haskell using this template repo,
-- so I was using my Coordinates `toPrintable` and just putting those in a nvim buffer
-- and replacing the newlines (:%s/\\n/\r/g)

import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy, signed, string)
import Data.Map (Map)
import Data.Set qualified as Set
import Program.RunDay qualified as R (Day, runDay)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = robot `sepBy` endOfLine
  where
    robot = do
      _ <- string "p="
      x <- decimal
      _ <- string ","
      y <- decimal
      _ <- string " v="
      vx <- signed decimal
      _ <- string ","
      vy <- signed decimal
      pure $ Robot (x, y) (vx, vy)

------------ TYPES ------------
type Input = [Robot]

type OutputA = Int

type OutputB = Int

data Robot = Robot
  { position :: (Int, Int),
    velocity :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

------------ PART 1 ------------
move :: (Int, Int) -> Int -> Robot -> Robot
move (bx, by) n (Robot (x, y) (vx, vy)) =
  Robot
    ( (x + n * vx) `mod` bx,
      (y + n * vy) `mod` by
    )
    (vx, vy)

quadrants :: (Int, Int) -> Input -> [Input]
quadrants (bx, by) robots = [q1, q2, q3, q4]
  where
    midX = bx `div` 2
    midY = by `div` 2
    q1 = filter (\r -> fst (position r) < midX && snd (position r) < midY) robots
    q2 = filter (\r -> fst (position r) > midX && snd (position r) < midY) robots
    q3 = filter (\r -> fst (position r) < midX && snd (position r) > midY) robots
    q4 = filter (\r -> fst (position r) > midX && snd (position r) > midY) robots

part1 :: Input -> OutputA
part1 =
  let boundary = (101, 103)
   in product . map length . quadrants boundary . map (move boundary 100)

------------ PART 2 ------------
allUnique :: (Int, Input) -> Bool
allUnique (_, xs) = Set.size unique == length xs
  where
    unique = Set.fromList $ map position xs

part2 :: Input -> OutputB
part2 input =
  -- just find the first where all robots are in unique positions
  -- maybe not a general solution but it works
  fst
    . head
    $ filter allUnique
    $ zip [0 ..]
    $ iterate (map (move (101, 103) 1)) input
