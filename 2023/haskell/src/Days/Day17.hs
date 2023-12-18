module Days.Day17 where

import Data.Attoparsec.Text (Parser, digit, endOfLine, many1, sepBy)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (mapBoundingBox, traceShowIdWithContext)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser mapper 0
  where
    mapper c = Just (digitToInt c)

------------ TYPES ------------
type Input = Map Coordinate Int

type OutputA = String

type OutputB = Void

type Costs = Map Coordinate Int

data Direction = North | West | South | East deriving (Enum, Show, Eq)

-- consider all directions equal for sorting
instance Ord Direction where
  compare x y = EQ

type Location = (Int, Coordinate, (Direction, Int))

-- the queue is tuples of the point, the distance to that point,
-- and how many times it's gone in a certain direction
type DijkstrasQueue = (Set Location, Map Coordinate Int)

------------ PART A ------------
rev North = South
rev South = North
rev West = East
rev East = West

nextPoints :: Location -> [(Coordinate, (Direction, Int))]
nextPoints (_, (x, y), (dir, n)) =
  -- limit forward to 3 times same direction
  -- and remove the option to turn around
  filter
    (\(_, (d, n')) -> d /= rev dir && n' <= 3)
    $ map
      -- if going in same direction as input, increment by that amount
      (\p@(c, (d', x)) -> if d' == dir then (c, (d', x + n)) else p)
      [ ((x - 1, y), (West, 1)),
        ((x + 1, y), (East, 1)),
        ((x, y - 1), (North, 1)),
        ((x, y + 1), (South, 1))
      ]

-- re-implementing here to keep the extra data of num of straight
dijkstras :: Input -> Int
dijkstras i = dijkstras' (Set.singleton (0, (0, 0), (South, 0))) Map.empty
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
              . map (\(loc, d) -> (cost + (i Map.! loc), loc, d))
              . filter (flip Map.member i . fst)
              $ nextPoints c
          newCandidates = Set.union (Set.fromList candidates') rest
          candidateCosts = map (\(cost, coord, d) -> ((coord, d), cost)) candidates'
          newCosts = foldr (uncurry Map.insert) costs candidateCosts
       in if curr == dest
            then cost
            else dijkstras' newCandidates newCosts

part1 :: Input -> OutputA
part1 = show . dijkstras

------------ PART B ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
