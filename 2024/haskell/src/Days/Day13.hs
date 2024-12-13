module Days.Day13 where

-- linear equations!!!
-- I never took this in college or anything so I had to put Gemini into teacher mode
-- to help me walk through it. Did some new monad stuff in here with it's help
-- which is cool. I kinda like the mathy ones now that I know to look out for them immediately
-- and it wasn't a grid today! YIPPEE!

import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, count, decimal, endOfLine, sepBy, string)
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = machine `sepBy` count 2 endOfLine
  where
    machine = do
      _ <- string "Button A: X+"
      aX <- decimal
      _ <- string ", Y+"
      aY <- decimal
      endOfLine
      _ <- string "Button B: X+"
      bX <- decimal
      _ <- string ", Y+"
      bY <- decimal
      endOfLine
      _ <- string "Prize: X="
      prizeX <- decimal
      _ <- string ", Y="
      prizeY <- decimal
      pure Machine {a = (aX, aY), b = (bX, bY), prize = (prizeX, prizeY)}

------------ TYPES ------------
type Input = [Machine]

type OutputA = Int

type OutputB = Int

data Machine = Machine
  { a :: (Int, Int),
    b :: (Int, Int),
    prize :: (Int, Int)
  }
  deriving (Show)

------------ PART 1 ------------
solve :: Machine -> Maybe (Int, Int)
solve Machine {a = (ax, ay), b = (bx, by), prize = (px, py)} =
  do
    let det = (ax * by) - (bx * ay)
    guard (det /= 0) -- No unique solution. Parallel lines
    let a_numerator = (px * by) - (bx * py)
        b_numerator = (ax * py) - (px * ay)
    -- make sure numerators are divisible by the determinant
    guard (a_numerator `mod` det == 0)
    guard (b_numerator `mod` det == 0)

    let a = a_numerator `div` det
        b = b_numerator `div` det

    guard (a >= 0 && b >= 0)

    return (a, b)

cost (Just (a, b)) = (3 * a) + b
cost Nothing = 0

part1 :: Input -> OutputA
part1 = sum . map (cost . solve)

------------ PART 2 ------------
convert :: Machine -> Machine
convert Machine {a, b, prize = (px, py)} =
  let diff = 10_000_000_000_000
   in Machine {a, b, prize = (px + diff, py + diff)}

part2 :: Input -> OutputB
part2 input =
  let higher = map convert input
   in sum . map (cost . solve) $ higher
