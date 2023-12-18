module Days.Day17 where

-- woof this one got me good.
-- came up with a great gameplan and it worked great
-- EXCEPT I did a dumb by considering all the Direction's to compare to EQ
-- and didn't know that would effect how Map.insert compares them
-- so it wasn't inserting if something had the same cost but different direction
--
-- and then part 2 wasn't so bad but it's slooooow but I don't care to optimize

import Data.Attoparsec.Text (Parser, digit, endOfLine, many1, sepBy)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (mapBoundingBox)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser mapper 0
  where
    mapper c = Just (digitToInt c)

------------ TYPES ------------
type Input = Map Coordinate Int

type OutputA = Int

type OutputB = Int

data Direction = North | West | South | East deriving (Enum, Show, Eq, Ord)

type Location = (Int, Coordinate, (Direction, Int))

------------ PART A ------------
rev North = South
rev South = North
rev West = East
rev East = West

nextPoints :: Int -> Input -> Location -> [Location]
nextPoints part m (cost, (x, y), (dir, n)) =
  -- limit forward to 3 times same direction
  -- and remove the option to turn around
  filter
    (\(_, _, (d, n')) -> d /= rev dir && n' <= limit && n' >= inc)
    . map
      -- if going in same direction as input, increment by that amount
      (\p@(cost, c, (d', x)) -> if d' == dir then (cost, c, (d', x + n)) else p)
    $ concat
      [ addCosts [((x - z, y), (West, z)) | z <- [1 .. limit]],
        addCosts [((x + z, y), (East, z)) | z <- [1 .. limit]],
        addCosts [((x, y - z), (North, z)) | z <- [1 .. limit]],
        addCosts [((x, y + z), (South, z)) | z <- [1 .. limit]]
      ]
  where
    addCosts :: [(Coordinate, (Direction, Int))] -> [Location]
    addCosts =
      drop 1
        . scanl
          ( \(prev, _, _) (c, d) ->
              (prev + Map.findWithDefault 0 c m, c, d)
          )
          (cost, (0, 0), (East, 0))
    limit = if part == 1 then 3 else 10
    inc = if part == 1 then 1 else 4

-- re-implementing here to keep the extra data of num of straight
dijkstras :: Int -> Input -> Int
dijkstras part i = dijkstras' (Set.singleton (0, (0, 0), (South, 0))) Map.empty
  where
    (_, maxX, _, maxY) = mapBoundingBox i
    dest = (maxX, maxY)
    isCheaper m (cost, p, d) = not (Map.member (p, d) m) || (cost < m Map.! (p, d))
    -- costs is map of ((x,y), (dir, count)) -> cost
    dijkstras' :: Set Location -> Map (Coordinate, (Direction, Int)) Int -> Int
    dijkstras' candidates costs =
      let (c@(cost, curr, _), rest) = Set.deleteFindMin candidates
          candidates' =
            filter (isCheaper costs)
              . filter (\(_, p, _) -> Map.member p i)
              $ nextPoints part i c
          newCandidates = Set.union (Set.fromList candidates') rest
          candidateCosts = map (\(cost, coord, d) -> ((coord, d), cost)) candidates'
          newCosts = foldr (uncurry Map.insert) costs candidateCosts
       in if curr == dest
            then cost
            else dijkstras' newCandidates newCosts

part1 :: Input -> OutputA
part1 = dijkstras 1

------------ PART B ------------
part2 :: Input -> OutputB
part2 = dijkstras 2
