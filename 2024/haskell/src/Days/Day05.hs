module Days.Day05 where

-- hooooly moly this is a funny one for me.
-- I really modeled this problem wrong at first.
-- I basically recorded everything as ordered sets, and then went
-- through and checked if the rest of the set had any that the first one
-- needed to be before. This worked great for part 1, but then part 2 needed them sorted.
--
-- so I tried to re-use the part1 parts by checking but it just wasn't really making sense.
-- then I realized all I had to do was write a sort comparator and give that to sortBy.
-- and I first did that by getting the frequencies of the numbers in the rules since the
-- way they were written in the example had them sort of descending (eg. 97 appears 6 times
-- on the left side, meaning it should take precendence over evertyhing else)
--
-- and then saw immediately this was a bad assumption because it didn't give me the same answer
-- on part 1.
--
-- so back to the drawing board! wasn't too long before it was just "oh compare by looking up the
-- other number and see if it is in it's set of numbers it must come before" and let the sorting algo
-- do all the swapping and whatnot.
--
-- ends up a really concise solution which is cool but annoying that it took me like 3 hours to get there!
--
-- lesson learned.

import Data.Attoparsec.Text (Parser, char, count, decimal, endOfLine, sepBy)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  rules <- rule `sepBy` endOfLine
  count 2 endOfLine
  updates <- update `sepBy` endOfLine
  return
    ( foldr (\(a, b) -> M.insertWith S.union b (S.fromList [a])) M.empty rules
    , filter (not . null) updates
    )
 where
  rule = do
    a <- decimal
    char '|'
    b <- decimal
    return (a, b)
  update = decimal `sepBy` ","

------------ TYPES ------------
type Input = (Rules, [[Int]])

type OutputA = Int

type OutputB = Int

-- a map of a number to all the numbers it must be after
type Rules = Map Int (Set Int)

------------ PART 1 ------------
middle :: [Int] -> Int
middle set =
  let size = length set
   in set !! (size `div` 2)

sort' :: Rules -> Int -> Int -> Ordering
sort' rules a b
  | b `S.member` x = GT
  | otherwise = LT
 where
  x = M.findWithDefault S.empty a rules

part1 :: Input -> OutputA
part1 (rules, updates) =
  sum
    . map middle
    $ mapMaybe
      ( \update ->
          let sorted = sortBy (sort' rules) update
           in if sorted == update then Just sorted else Nothing
      )
      updates

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 (rules, updates) = sum $ map middle fixed
 where
  fixed =
    mapMaybe
      ( \update ->
          let sorted = sortBy (sort' rules) update
           in if sorted == update then Nothing else Just sorted
      )
      updates
