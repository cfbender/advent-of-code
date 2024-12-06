module Days.Day06 where

-- BRUTE FORCING AND OFF BY ONE ERRORS WOOO IT'S A BEAUTIFUL FRIDAY!!!!
-- really thought this one was fun and is a great example of how Haskell is
-- such a good fit for these problems.
--
-- still couldn't figure out how to do something like `reduce_while` that elixir
-- has in Haskell, but the `unfoldr` was nice enough knowing I could just add one to the count
-- and hoping I didn't need to find at what point the guard left in part 2 (gambit successful)
--
-- I think some of my interface design still could be improved, I'm not thrilled with 3
-- or 4 arity functions, but it's okay it's just a little calendar of coding challenges

-- another where you probably ought to run this with
-- stack run --RTS -- -d 6 +RTS -N10
-- (replace the 10 with number of cores)
-- even with that for me this still takes like 30s so idk if the RTS opts are wrong
-- or my solution is just that slow but it's good enough for me!

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  -- pop out the guard location and replace it with an empty space
  M.foldrWithKey
    ( \k v (v', acc) ->
        case v of
          Guard -> (k, M.insert k Empty acc)
          _ -> (v', M.insert k v acc)
    )
    ((0, 0), M.empty)
    <$> coordinateParser mapper 0
 where
  mapper '.' = Just Empty
  mapper '#' = Just Obstacle
  mapper '^' = Just Guard
  mapper _ = Nothing

------------ TYPES ------------
type PuzzleMap = Map Coordinate Location

type Input = (Coordinate, PuzzleMap)

type OutputA = Int

type OutputB = Int

data Location = Guard | Obstacle | Empty deriving (Show, Eq, Ord)
data Direction = North | East | South | West deriving (Show, Eq, Ord)

------------ PART 1 ------------
next :: Coordinate -> Direction -> Coordinate
next (x, y) North = (x, y - 1)
next (x, y) East = (x + 1, y)
next (x, y) South = (x, y + 1)
next (x, y) West = (x - 1, y)

turn :: Direction -> Direction
turn North = East
turn East = South
turn South = West
turn West = North

check :: PuzzleMap -> Coordinate -> Direction -> Maybe (Coordinate, Direction)
check m guard d =
  let nextPoint = next guard d
   in case m M.!? nextPoint of
        Just Obstacle -> Just (guard, turn d)
        Just Empty -> Just (nextPoint, d)
        Nothing -> Nothing

part1 :: Input -> OutputA
part1 (guard, input) =
  -- add one for dropping the final location
  (+ 1) $ length . S.fromList $ guard : unfoldr step (guard, North)
 where
  step (coord, dir) = case check input coord dir of
    Just (nextCoord, nextDir) -> Just (coord, (nextCoord, nextDir))
    Nothing -> Nothing

------------ PART 2 ------------
detectCycle' :: PuzzleMap -> (Coordinate, Direction) -> Set (Coordinate, Direction) -> Bool
detectCycle' m (coord, dir) seen =
  case check m coord dir of
    Just (nextCoord, nextDir) ->
      -- return True if we've seen this location and direction before
      S.member (nextCoord, nextDir) seen
        || detectCycle' m (nextCoord, nextDir) (S.insert (nextCoord, nextDir) seen)
    Nothing -> False

detectCycle :: PuzzleMap -> (Coordinate, Direction) -> Coordinate -> Maybe Coordinate
detectCycle m guard p =
  let m' = M.insert p Obstacle m
   in if detectCycle' m' guard S.empty
        then Just p
        else Nothing

part2 :: Input -> OutputB
-- map over all empties
-- put an obstruction in
-- check through and stop if returns identical (coordinate, direction)
part2 (guard, input) =
  length $
    filter isJust $
      parMap rdeepseq (detectCycle input (guard, North)) empties
 where
  empties = M.keys $ M.filter (== Empty) input
