module Days.Day16 where

-- this was a really fun one!
--
-- got super tripped up on trying to determine the steady state,
-- tried a lot of iterate and cycle detection
-- but got some help and once I got that it was no problem.
-- did not expect my input to start with a mirror at (0,0), that
-- messed me up big time.
--
-- this solution is pretty slow but don't really feel like optimizing

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine)
import Control.Monad.List (foldM)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Util.Parsers (coordinateParser, Coordinate)
import qualified Util.Util as U
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser tile 0
  where
    tile '.' = Just (Empty, False)
    tile '/' = Just (RMirror, False)
    tile '\\' = Just (LMirror, False)
    tile '|' = Just (VSplitter, False)
    tile '-' = Just (HSplitter, False)

------------ TYPES ------------
data Tile = Empty | RMirror | LMirror | VSplitter | HSplitter deriving (Show, Eq)

type Beam = (Coordinate, Coordinate)

type Input = Map Coordinate (Tile, Bool)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
encounter :: Input -> Beam -> [Beam]
encounter i b@((x, y), (dx, dy)) = maybe [] (encounter' b . fst) tile
  where
    tile = i M.!? (x + dx, y + dy)

encounter' :: Beam -> Tile -> [Beam]
encounter' ((x, y), d@(dx, dy)) Empty = [((x + dx, y + dy), d)]
encounter' ((x, y), (dx, dy)) RMirror
  | dx == 1 = [((x + 1, y), (0, -1))]
  | dx == -1 = [((x - 1, y), (0, 1))]
  | dy == 1 = [((x, y + 1), (-1, 0))]
  | dy == -1 = [((x, y - 1), (1, 0))]
encounter' beam LMirror = [(b, (dx * (-1), dy * (-1)))]
  where
    [(b, (dx, dy))] = encounter' beam RMirror
encounter' ((x, y), d@(dx, dy)) VSplitter
  | dy /= 0 = [((x + dx, y + dy), d)]
  | otherwise = [((x + dx, y), (0, 1)), ((x + dx, y), (0, -1))]
encounter' ((x, y), d@(dx, dy)) HSplitter
  | dx /= 0 = [((x + dx, y + dy), d)]
  | otherwise = [((x, y + dy), (1, 0)), ((x, y + dy), (-1, 0))]

-- takes map of tiles, and a tuple of:
--      a set of beams we have seen before
--      and a list of beams as a tuple of (location, (dx, dy))
step :: Input -> (Set Beam, [Beam]) -> Either (Set Beam, [Beam]) (Set Beam, [Beam])
step m (s, b) = if null beams then Left (s, b) else Right (visited, beams)
  where
    beams = concatMap (filter (\b -> not (S.member b s)) . encounter m) b
    visited = foldr S.insert s beams

energized :: Input -> (Set Beam, [Beam]) -> Int
energized i init =
  length
    . S.map fst
    . fst
    . either id id
    $ foldM (\a _ -> step i a) init [0 ..]

part1 :: Input -> OutputA
part1 i = energized i init
  where
    beam = ((-1, 0), (1, 0))
    init = (S.empty, [beam])

------------ PART B ------------
part2 :: Input -> OutputB
part2 i = maximum $ map (energized i) starts
  where
    (_, maxX, _, maxY) = U.mapBoundingBox i
    starts = lefts ++ rights ++ tops ++ bottoms
    lefts = [(S.empty, [((-1, y), (1, 0))]) | y <- [0 .. maxY]]
    rights = [(S.empty, [((maxX + 1, y), (-1, 0))]) | y <- [0 .. maxY]]
    tops = [(S.empty, [((x, -1), (0, 1))]) | x <- [0 .. maxX]]
    bottoms = [(S.empty, [((x, maxY + 1), (0, -1))]) | x <- [0 .. maxX]]
