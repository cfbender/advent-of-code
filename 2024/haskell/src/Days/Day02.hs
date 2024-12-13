module Days.Day02 where

-- Not a bad day 2, pretty fun one! Still just a lot of time fighting the compiler
-- as I ride the bumpy start of learning Haskell again. Loving it tho, it writes such pretty code
-- even when my example here is pretty ugly.
-- Onward!

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.Ix
import Data.List
import Data.Maybe
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = filter ((> 1) . length) <$> report `sepBy` endOfLine
  where
    report = decimal `sepBy` char ' '

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

data Finding = Safe | Unsafe
  deriving (Show, Eq)

------------ PART 1 ------------
valid :: Int -> Int -> Bool
valid x y = inRange (1, 3) (abs (x - y))

check :: Finding -> [Int] -> Finding
check Unsafe _ = Unsafe
check Safe (x : y : z : xs) = check safety (y : z : xs)
  where
    safe = valid x y && valid y z
    safety = if safe && ((z > y && y > x) || (z < y && y < x)) then Safe else Unsafe
-- getting to a 2 length list means we've compared all the windows
check Safe [_, _] = Safe

part1 :: Input -> OutputA
part1 = length . filter (== Safe) . map (check Safe)

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 input = length $ mapMaybe (find (== Safe)) results
  where
    results :: [[Finding]]
    results = map (map (check Safe)) tests
    tests :: [[[Int]]]
    -- get all the permutations of removing an element
    tests = map permute input
    permute :: [Int] -> [[Int]]
    -- generate combinations including the original list to check first
    permute xs = xs : [deleteAt n xs | n <- [0 .. length xs - 1]]
    deleteAt n xs =
      let (ys, zs) = splitAt n xs
       in ys ++ tail zs
