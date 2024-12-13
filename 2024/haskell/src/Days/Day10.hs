module Days.Day10 where

-- lol this was a funny one. I was underthinking part 1
-- and then realized it needed to be set logic to not overcount
-- unique points. Along the way in part 1 I got the answer "[5,5,20,1,24,10,4,4,8]"
-- and obviously it was wrong so I refactored into the Set solution I have now.
--
-- but then when I read part 2 I was like "wait those numbers look familiar"
-- and just went back into the undo history to find that old implementation of hike
-- and it just worked! yay!
--
-- that's a first. acidentally solving part 2 while working on part 1.

import Data.Attoparsec.Text (Parser, digit, endOfLine, sepBy)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (neighborsNoCorners)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser mapper 0
  where
    mapper x = Just (digitToInt x)

------------ TYPES ------------
type Input = Map Coordinate Int

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------

next :: Input -> Coordinate -> [(Coordinate, Int)]
next input x = mapMaybe check $ neighborsNoCorners x
  where
    start = input M.! x
    check x = do
      let val = input M.!? x
      -- only include values that are in the map and increment by one
      if val == Just (start + 1)
        then Just (x, start + 1)
        else Nothing

hike :: Input -> Set Coordinate -> Coordinate -> Set Coordinate
hike input acc x = case next input x of
  [] -> acc
  xs ->
    let (nines, checks) = U.splitWith ((== 9) . snd) xs
        -- if we found any nines, split them out and add them to seen ends
        ends = acc `S.union` S.fromList (map fst nines)
     in -- recurse with everything not a nine
        foldr (S.union . hike input ends . fst) ends checks

part1 :: Input -> OutputA
part1 input = sum $ map (S.size . hike input S.empty) trailheads
  where
    trailheads = M.keys $ M.filter (== 0) input

------------ PART 2 ------------
hike' :: Input -> Coordinate -> Int
hike' input x = case next input x of
  [] -> 0
  xs ->
    -- find all the points that ended with a 9
    let ends = length $ filter ((== 9) . snd) xs
     in -- recurse with the rest
        ends + sum (map (hike' input . fst) xs)

part2 :: Input -> OutputB
part2 input = sum $ map (hike' input) trailheads
  where
    trailheads = M.keys $ M.filter (== 0) input
