module Days.Day11 where

-- this was a fun one! the first one where I stopped early at night
-- and took some time to think before coming back to it. so I came up
-- with the math-only solution here and I'm pretty happy with it.
--
-- messed around with vectors for a bit and didn't need em, but good to learn them


import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser)
import Data.List
import Util.Parsers (coordinateParser)
import qualified Data.Map as Map


runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.toList <$> coordinateParser space 0
  where
    space '#' = Just Galaxy
    space '.' = Just Empty

------------ TYPES ------------
data Space = Empty | Galaxy deriving (Show, Eq)

type Coordinate = (Int, Int)

type Input = [(Coordinate, Space)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
manhattan :: Coordinate -> Coordinate -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

path :: Int -> ([Int], [Int]) -> Coordinate -> Coordinate -> Int
path d (ec, er) a@(x, y) b@(x', y') = manhattan a b + dx + dy
  where
    dx = d * length (filter (\c -> c > min x x' && c < max x x') ec)
    dy = d * length (filter (\r -> r > min y y' && r < max y y') er)

calculate d i = sum $ map (uncurry (path d (emptyCols, emptyRows))) pairs
  where
    galaxies = [c | (c, g) <- i, g == Galaxy]
    pairs = [(g, g') | (g : xs) <- tails galaxies, g' <- xs]
    transposed = groupBy (\((xA, _), _) ((xB, _), _) -> xA == xB) i
    emptyRows = map fst . filter (all ((== Empty) . snd) . snd) . zip [0 ..] $ transpose transposed
    emptyCols = map fst . filter (all ((== Empty) . snd) . snd) . zip [0 ..] $ transposed

part1 :: Input -> OutputA
part1 = calculate 1

------------ PART B ------------
part2 :: Input -> OutputB
part2 = calculate 999999
