module Days.Day07 where

-- a fun part 1, and then a difficult to understand and think through part 2
-- I, of course, tried the naive "store everything in a list" approach and it blows up.
-- So tried a set and then figured out the map with counts approach which worked well enough.

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates (mapBoundingBox)
import Util.Parsers (Coordinate, coordinateParser)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser grid 0
  where
    grid 'S' = Just Start
    grid '^' = Just Splitter
    grid _ = Nothing

------------ TYPES ------------
data Tachyon = Splitter | Beam | Start deriving (Show, Eq)
type Input = Map Coordinate Tachyon

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
split :: Coordinate -> [Coordinate] -> [Coordinate]
split l bs = new ++ bs
  where
    (x, y) = l
    new = [(x + 1, y), (x - 1, y)]

step :: Input -> [Coordinate] -> Int -> Int -> (Input, [Coordinate])
step m [] 0 xMax = (updated, beams)
  where
    (x, 0) : _ = [k | (k, v) <- M.toList m, v == Start]
    beams = [(x, 1)]
    updated = M.insert (x, 1) Beam m
step m bs y xMax = (updated, newBeams ++ bs)
  where
    splitters = map fst . filter (\(_, s) -> s == Just Splitter) $ [((x, y), M.lookup (x, y) m) | x <- [0 .. xMax]]
    beams = map fst . filter (\(_, b) -> b == Just Beam) $ [((x, y - 1), M.lookup (x, y - 1) m) | x <- [0 .. xMax]]
    move (x, y') = (x, y' + 1)
    moved = map move beams
    hits = filter (`elem` splitters) moved
    newBeams = foldr split moved hits
    updated = foldr (`M.insert` Beam) m (filter (`notElem` splitters) newBeams)

run :: Input -> [Coordinate] -> Int -> Int -> Int -> (Input, [Coordinate])
run m bs y yMax xMax =
    if y > yMax
        then (m, bs)
        else run m' bs' (y + 1) yMax xMax
  where
    (m', bs') = step m bs y xMax

part1 :: Input -> OutputA
part1 i = length $ filter (Just Splitter ==) $ map (`M.lookup` finalMap) finalBeams
  where
    (finalMap, finalBeams) = run i [] 0 yMax xMax
    (_, xMax, _, yMax) = mapBoundingBox i

------------ PART 2 ------------
countTimelines :: Input -> Int -> Map Coordinate Int -> Int
countTimelines m yMax beamCounts
    | M.null beamCounts = 0
    | otherwise =
        let (exited, active) = M.partitionWithKey (\(_, y) _ -> y > yMax) beamCounts
            exitCount = sum (M.elems exited)
            newBeamCounts = M.fromListWith (+) [(pos, count) | (oldPos, count) <- M.toList active, pos <- processBeam oldPos]
         in exitCount + countTimelines m yMax newBeamCounts
  where
    processBeam (x, y) =
        let next = (x, y + 1)
         in case M.lookup next m of
                Just Splitter -> [(x - 1, y + 1), (x + 1, y + 1)]
                _ -> [next]

part2 :: Input -> OutputB
part2 i = countTimelines i yMax (M.singleton (x, 1) 1)
  where
    (x, 0) : _ = [k | (k, v) <- M.toList i, v == Start]
    (_, xMax, _, yMax) = mapBoundingBox i
