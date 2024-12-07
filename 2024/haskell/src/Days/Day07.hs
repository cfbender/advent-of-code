module Days.Day07 where

-- god I love when I can just keep brute forcing
-- this one is actually reasonable with the parallelization, only takes a second or two.
-- without it takes like a minute which is not too bad
--
-- pretty fun one tho! really pseudocoded this one beforehand while I was waiting on my car service
-- which was pretty fun. I was thinking through it and then wrote: "Actually just literally generate all permutations fuck it"
-- which worked really well. saw that it was probably gonna be at worst like 2^12 combinations to check which wasn't too bad.
-- ofc 3^12 was considerably worse but still doable! yay! probably the last time this year we get to do that

import Control.Monad
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy, space)
import Data.Foldable (find, foldl')
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = calibration `sepBy` endOfLine
 where
  calibration = do
    total <- decimal
    char ':'
    space
    nums <- decimal `sepBy` char ' '
    return (total, nums)

------------ TYPES ------------
type Input = [(Int, [Int])]

type OutputA = Int

type OutputB = Int

data Operation = Mul | Add | Concat deriving (Show, Eq)

------------ PART 1 ------------
combos :: Int -> [[Operation]]
combos n = replicateM n [Mul, Add]

calculate :: Int -> (Int, Operation) -> Int
calculate x (y, Mul) = x * y
calculate x (y, Add) = x + y
calculate x (y, Concat) = read (show x ++ show y)

check' :: [Int] -> [Operation] -> Int
check' ns ops = foldl' calculate 0 (zip ns ops)

check :: (Int -> [[Operation]]) -> Int -> [Int] -> Maybe Int
check checkMapper x ns = case find ((== x) . check' ns) $ checkMapper (length ns) of
  Just _ -> Just x
  _ -> Nothing

part1 :: Input -> OutputA
part1 = sum . mapMaybe (uncurry (check combos))

------------ PART 2 ------------
combos2 :: Int -> [[Operation]]
combos2 n = replicateM n [Mul, Add, Concat]

part2 :: Input -> OutputB
part2 = sum . mapMaybe (uncurry (check combos2))

-- uncomment to run parallelized with
-- stack run --RTS -- -d 6 +RTS -N10
--
-- part2 input =
--   sum . catMaybes $
--     parMap rdeepseq (uncurry (check combos2)) input
