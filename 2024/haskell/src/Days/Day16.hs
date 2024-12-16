module Days.Day16 where

-- part 1:
-- classic dijkstras, just copied out my old implementation.
-- pretty fun one. I think this may be where I start really only doing part 1s or ones where p2 seems easy.
-- it's been a good run these last 4 years, but maybe it's time to not commit to this whole month

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates (Direction (E, N, S, W), neighborsNoCorners)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (third)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  grid <- coordinateParser mapper 0
  return (process grid)
  where
    get x = head . Map.keys . Map.filter (== x)
    process :: Map Coordinate Tile -> Input
    process x =
      let start = get Start x
          end = get End x
          replaced = Map.insert start Empty $ Map.insert end Empty x
       in (start, end, replaced)
    mapper '#' = Just Wall
    mapper '.' = Just Empty
    mapper 'S' = Just Start
    mapper 'E' = Just End

------------ TYPES ------------
type Input = (Coordinate, Coordinate, Map Coordinate Tile)

type OutputA = Int

type OutputB = Void

data Tile = Start | End | Empty | Wall deriving (Show, Eq)

------------ PART 1 ------------
moves :: Map Coordinate Tile -> Coordinate -> Direction -> [(Int, Direction, Coordinate)]
moves m c d = filter (\(_, _, x) -> (== Just Empty) $ m Map.!? x) $ moves' c d
  where
    moves' (x, y) N = [(1, N, (x, y - 1)), (1001, E, (x + 1, y)), (1001, W, (x - 1, y))]
    moves' (x, y) E = [(1, E, (x + 1, y)), (1001, N, (x, y - 1)), (1001, S, (x, y + 1))]
    moves' (x, y) S = [(1, S, (x, y + 1)), (1001, E, (x + 1, y)), (1001, W, (x - 1, y))]
    moves' (x, y) W = [(1, W, (x - 1, y)), (1001, N, (x, y - 1)), (1001, S, (x, y + 1))]

-- reimplementing to carry forward direction
dijkstras :: (Direction, Coordinate) -> Coordinate -> Map Coordinate Tile -> Int
dijkstras start end i = dijkstras' (Set.singleton (0, start)) Map.empty
  where
    isCheaper m (cost, p) = not (Map.member p m) || (cost < m Map.! p)
    dijkstras' candidates costs =
      let (c@(cost, (dir, curr)), rest) = Set.deleteFindMin candidates
          candidates' =
            filter (isCheaper costs)
              . map (\(extra, dir, loc) -> (cost + extra, (dir, loc)))
              $ moves i curr dir
          newCandidates = Set.union (Set.fromList candidates') rest
          candidateCosts = map (\(cost, coord) -> (coord, cost)) candidates'
          newCosts = foldr (uncurry Map.insert) costs candidateCosts
       in if curr == end
            then cost
            else dijkstras' newCandidates newCosts

part1 :: Input -> OutputA
part1 (start, end, input) = dijkstras (E, start) end input

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
