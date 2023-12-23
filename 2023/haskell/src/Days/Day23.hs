module Days.Day23 where

-- okay this shit took like 4 hours to run
-- but I don't feel like fixing it right now.
--
-- if I do, note to future self: implement this as a graph
-- and preprocess the edges to skip sections of the map
-- where there are no intersections
-- (something like, if the next tiles is a 1 length list then keep going until it isn't)
--
-- maybe will be a fun thing to write later but I just don't have the energy for part 2s
-- right now

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (neighborsNoCorners)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser trail 0
  where
    trail '.' = Just Path
    trail '#' = Nothing
    trail '^' = Just $ Slope U
    trail '>' = Just $ Slope R
    trail 'v' = Just $ Slope D
    trail '<' = Just $ Slope L

------------ TYPES ------------
data Hill = L | R | U | D deriving (Show, Eq)

data Tile = Slope Hill | Path deriving (Show, Eq)

type Input = Map Coordinate Tile

type OutputA = Int

type OutputB = Int

------------ PART A ------------
nextTiles :: Coordinate -> Tile -> [Coordinate]
nextTiles (x, y) (Slope L) = [(x - 1, y)]
nextTiles (x, y) (Slope R) = [(x + 1, y)]
nextTiles (x, y) (Slope U) = [(x, y - 1)]
nextTiles (x, y) (Slope D) = [(x, y + 1)]
nextTiles (x, y) Path = neighborsNoCorners (x, y)

longestPath :: Coordinate -> Coordinate -> Input -> Bool -> Int
longestPath start end i all = dijkstras' (S.singleton (0, start, S.singleton start)) 0
  where
    isLonger m (cost, p) = not (M.member p m) || (cost > m M.! p)
    dijkstras' candidates highest =
      let (c@(cost, curr, seen), rest) = S.deleteFindMax candidates
          candidates' =
            map (\loc -> (cost + 1, loc, S.insert loc seen))
              . filter
                ( \loc ->
                    loc `M.member` i
                      && (loc `S.notMember` seen)
                )
              $ tiles
          tiles = if all then neighborsNoCorners curr else nextTiles curr (i M.! curr)
          newCandidates = S.union (S.fromList candidates') rest
          newCost = if curr == end then max cost highest else highest
       in if S.null candidates
            then highest
            else dijkstras' newCandidates newCost

part1 :: Input -> OutputA
part1 i = longestPath start end i False
  where
    (start, _) = M.findMin i
    (end, _) = M.findMax i

------------ PART B ------------
part2 :: Input -> OutputB
part2 i = longestPath start end i True
  where
    (start, _) = M.findMin i
    (end, _) = M.findMax i
