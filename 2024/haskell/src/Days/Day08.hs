module Days.Day08 where

-- fun lil puzzle! really happy with how concise this turned out,
-- just parameterizing between parts 1 and 2 is always a pleasure.
--
-- got hung up on some silly errors of overfiltering (not uniquing but excluding those
-- that were antennas gave me correct for sample but wrong for full) - but figured it out.
-- Then just had to do the point math for checking
-- more points along the slope for antinodes.
--
-- Feel like these are about to start getting harder now which sucks.
-- These first week problems are always my favorites

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.List (subsequences)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (groupBy)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser point 0
  where
    point '.' = Just Empty
    point x = Just (Antenna x)

------------ TYPES ------------
type Input = Map Coordinate Point

type OutputA = Int

type OutputB = Int

data Point = Antenna Char | Empty deriving (Show, Eq, Ord)

------------ PART 1 ------------
distance :: (Coordinate, Coordinate) -> Coordinate
distance ((x1, y1), (x2, y2)) = (x1 - x2, y1 - y2)

antinodes :: Int -> (Coordinate, Coordinate) -> [Coordinate]
antinodes n (a, b) =
  concat
    [ [ (x1 + (d * dx), y1 + (d * dy)),
        (x2 - (d * dx), y2 - (d * dy))
      ]
      | d <- multipliers
    ]
  where
    (dx, dy) = distance (a, b)
    (x1, y1) = a
    (x2, y2) = b
    -- only count the point itself if checking outside of 1 range (part 2)
    multipliers = if n == 1 then [1] else [0 .. n]

solve :: ((Coordinate, Coordinate) -> [Coordinate]) -> Input -> Int
solve generator input =
  length $
    S.fromList $
      concat $
        pairs >>= map (filter (`M.member` input) . generator)
  where
    -- group across the map by antenna name
    antennas = M.map (map fst) . groupBy snd $ M.toList $ M.filter (/= Empty) input
    -- map across each antenna to get all the combinations of points into tuples
    pairs =
      map
        ( map
            (\(a : b : _) -> (a, b))
            . filter ((2 ==) . length)
            . subsequences
            . snd
        )
        (M.toList antennas)

part1 :: Input -> OutputA
part1 = solve (antinodes 1)

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = solve (antinodes 50)
