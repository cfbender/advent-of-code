module Days.Day19 where

-- weee I did another part 2! tried to just brute force, but I think that was near impossible.
-- so memoization again! I just used a library now since I've implemented that enough

import Data.Attoparsec.Text (Parser, count, endOfLine, letter, many1, sepBy)
import Data.MemoCombinators (char, list, memo2)
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  patterns <- many1 letter `sepBy` ", "
  count 2 endOfLine
  designs <- many1 letter `sepBy` endOfLine
  return (patterns, designs)

------------ TYPES ------------
type Input = ([String], [String])

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
match :: String -> String -> Bool
match d p = (==) p $ take (length p) d

check' :: [String] -> String -> Int
check' _ [] = 1
check' patterns design = sum $ map (check patterns) rest
  where
    matches = filter (match design) patterns
    rest = map (\m -> drop (length m) design) matches

check :: [String] -> String -> Int
check = memo2 (list (list char)) (list char) check'

part1 :: Input -> OutputA
part1 (patterns, designs) = length $ filter ((/= 0) . check patterns) designs

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 (patterns, designs) = sum $ map (check patterns) designs
