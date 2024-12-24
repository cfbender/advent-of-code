module Days.Day22 where

-- fun little algo in part 1 - wtf is the part 2??
-- probably some math trick ugh

import Control.Monad (replicateM)
import Data.Array (Array, (!))
import Data.Array.Base (listArray)
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import Data.Bits
import Data.List (elemIndex, maximumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Util (windows)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = String

------------ PART 1 ------------

next :: Int -> Int
next i = step3 . step2 . step1 $ i
  where
    prune x = x `mod` 16777216
    step1 x = prune . xor x $ (* 64) i
    step2 x = prune $ xor x (x `div` 32)
    step3 x = prune $ xor x (x * 2048)

nth :: Int -> Int -> Int
nth n i = iterate next i !! n

part1 :: Input -> OutputA
part1 = sum . map (nth 2000)

------------ PART 2 ------------
generate :: Int -> Array Int Int
generate start = listArray (0, 2000) $ take 2001 $ map (`mod` 10) $ iterate next start

getChanges :: Array Int Int -> Array Int Int
getChanges arr = listArray (0, 1999) [arr ! (i + 1) - arr ! i | i <- [0 .. 1999]]

findFirst :: [Int] -> Array Int Int -> Maybe Int
findFirst pattern changes =
  let matches =
        [ i | i <- [0 .. 1995], and [changes ! (i + j) == pattern !! j | j <- [0 .. 3]]
        ]
   in case matches of
        (i : _) -> Just (changes ! (i + 4))
        [] -> Nothing

findBest :: [Int] -> ([Int], Int)
findBest initials =
  let sequences = map (getChanges . generate) initials
      patterns = replicateM 4 [-9 .. 9]
      score pat = (pat, sum $ mapMaybe (findFirst pat) sequences)
   in maximumBy (comparing snd) $ map score patterns

part2 :: Input -> OutputB
part2 = show . findBest
