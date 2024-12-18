module Days.Day18 where

--- weeee I did a part 2!
-- I spent sooo much time on part 1 thinking that a byte fell every time you stepped
-- idk why but it wasn't just me. the problem as written just seemed too straightforward
-- then luckily the naive approach works on part 2, it just is really slow

import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import Data.Bifunctor (second)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates qualified as C
import Util.Parsers (Coordinate)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = byte `sepBy` endOfLine
  where
    byte = do
      x <- decimal
      _ <- ","
      y <- decimal
      pure (x, y)

------------ TYPES ------------
type Input = [Coordinate]

type OutputA = Int

type OutputB = Coordinate

data Tile = Corrupted | Empty deriving (Show, Eq)

------------ PART 1 ------------
corrupt :: Map Coordinate Tile -> Coordinate -> Map Coordinate Tile
corrupt m c = Map.insert c Corrupted m

bound = 70

emptyGrid =
  Map.fromList
    . flip zip (repeat Empty)
    $ [(x, y) | x <- [0 .. bound], y <- [0 .. bound]]

cost :: Int -> Tile -> Tile -> Int
cost x _ b
  | b == Empty = x + 1
  | otherwise = x + 1_000_000

part1 :: Input -> OutputA
part1 input =
  C.dijkstras
    (0, 0)
    (bound, bound)
    cost
    grid
  where
    bytes = take 1024 input
    grid = foldl corrupt emptyGrid bytes

------------ PART 2 ------------

part2 :: Input -> OutputB
part2 input = case find ((> 1_000_000) . snd) $
  map (second (C.dijkstras (0, 0) (bound, bound) cost)) grids of
  Just (c, _) -> c
  Nothing -> (-1, -1)
  where
    -- add an extra value to offset the list by one
    -- which will get the previous block that has made that path impossible
    grids = zip ((-1, -1) : input) $ scanl corrupt emptyGrid input
