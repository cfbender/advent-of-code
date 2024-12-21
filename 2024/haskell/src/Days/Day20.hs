module Days.Day20 where

import Control.Parallel.Strategies (parMap, rdeepseq, rseq)
import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates (neighborsNoCorners, rays)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser mapper 0
 where
  mapper '#' = Just Wall
  mapper '.' = Just Track
  mapper 'S' = Just Start
  mapper 'E' = Just End

------------ TYPES ------------
type Input = Map Coordinate Tile

type OutputA = Int

type OutputB = Void

data Tile = Wall | Track | Start | End
  deriving (Show, Eq)

-- (hasCheated, currPos, time, seen)
type RaceState = (Bool, Coordinate, Int, Set Coordinate)

------------ PART 1 ------------
filterMoves :: Input -> Set Coordinate -> [RaceState] -> [RaceState]
filterMoves i seen = filter (\(_, x, _, _) -> not (Set.member x seen) && isPossible i x)
 where
  isPossible i x =
    let next = i Map.!? x
     in next == Just Track || next == Just End

moves :: Input -> RaceState -> [RaceState]
moves i (True, c, t, seen) =
  filterMoves i seen
    . map (True,,t + 1,Set.insert c seen)
    $ neighborsNoCorners c
moves i (False, c, t, seen) = filterMoves i seen cheats
 where
  cheats =
    concatMap
      ( \(a : b : _) ->
          [(False, a, t + 1, Set.insert c seen), (True, b, t + 2, Set.insert c seen)]
      )
      $ take 4
      $ rays 2 c

race :: Input -> (Bool, Coordinate, Int, Set Coordinate) -> [Int]
race i (hc, c, t, s) = case Map.lookup c i of
  Just End -> [t]
  _ -> concat $ parMap rseq (race i) next
 where
  next = moves i (hc, c, t, s)

part1 :: Input -> OutputA
part1 input =
  length
    . filter (>= 100)
    $ map (base -)
    $ race input (False, start, 0, Set.empty)
 where
  start = fst . head . Map.toList $ Map.filter (== Start) input
  (base : _) = race input (True, start, 0, Set.empty)

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
