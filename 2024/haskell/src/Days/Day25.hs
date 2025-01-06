module Days.Day25 where

-- fun little puzzle here! pretty happy with how concise this is.
-- not sure if I'll do any more of 2024, this felt like a really healthy way to do it
-- for me finally and I loved it. looking forward to practicing more haskell in 2025

import Data.Attoparsec.Text (Parser, anyChar, choice, count, endOfLine, many1, sepBy)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Void
import Days.Day15 (coordinateParser)
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate)
import Util.Parsers qualified as P
import Util.Util (splitWith)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  head' <- many1 (coordinateParser pure 0)
  last' <- P.coordinateParser pure 0
  return (head' ++ [last'])

------------ TYPES ------------
type Input = [Map Coordinate Char]

type OutputA = Int

type OutputB = Void

------------ PART 1 ------------

heights :: [Int] -> Map Coordinate Char -> [Int]
heights checks input = map (fromJust . height) [0 .. 4]
  where
    height x = find (\i -> input Map.! (x, i) == '#') checks

overlap :: [Int] -> [Int] -> Bool
overlap a b = any (> 5) $ zipWith (+) a b

part1 :: Input -> OutputA
part1 input = length . filter not $ [overlap a b | a <- lockHeights, b <- keyHeights]
  where
    (locks, keys) = splitWith isLock input
    isLock i = (== '#') $ i Map.! (0, 0)
    lockHeights = map (heights [5, 4 .. 0]) locks
    keyHeights = map (map (6 -) . heights [1 .. 6]) keys

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
