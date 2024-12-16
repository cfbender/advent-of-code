module Days.Day15 where

import Data.Attoparsec.Text (Parser, anyChar, choice, count, endOfLine, many1, skipSpace)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates (Direction (E, N, S, W), move)
import Util.Parsers (Coordinate)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
-- re-implementation of coordinateParser to check for 2 endOfLine first
coordinateParser :: (Char -> Maybe a) -> Int -> Parser (Map Coordinate a)
coordinateParser mapper start = coordinateParser' start start
  where
    coordinateParser' x y =
      choice
        -- First check if we've reached the end of the grid
        [ count 2 endOfLine >> return Map.empty,
          -- Look for a line break, and reset coordinates appropriately
          endOfLine >> coordinateParser' start (y + 1),
          -- Then look for a character, and map it
          anyChar >>= (\c -> addToMap mapper x y c <$> coordinateParser' (x + 1) y)
        ]
    addToMap mapper x y c = Map.alter (const (mapper c)) (x, y)

inputParser :: Parser Input
inputParser = do
  grid <- coordinateParser mapper 0
  skipSpace
  instructions <- filter (/= '\n') <$> many1 anyChar
  return
    ( grid,
      fst . fromJust . find ((==) Robot . snd) $ Map.toList grid,
      map instruction instructions
    )
  where
    removeRobot v = if v == Robot then Empty else v
    mapper '#' = Just Wall
    mapper '.' = Just Empty
    mapper 'O' = Just Box
    mapper '@' = Just Robot
    instruction '^' = N
    instruction 'v' = S
    instruction '>' = E
    instruction '<' = W

------------ TYPES ------------
type Input = (Map Coordinate Tile, Coordinate, [Direction])

type OutputA = Int

type OutputB = Void

data Tile = Wall | Empty | Box | Robot
  deriving (Show, Eq)

------------ PART 1 ------------
push :: (Tile, Coordinate) -> Map Coordinate Tile -> Direction -> (Coordinate, Map Coordinate Tile)
push (p, pusher) input dir =
  let next = move pusher dir
      nextTile = input Map.!? next
   in case nextTile of
        -- if pusher tries to move to wall, do nothing
        Just Wall -> (pusher, input)
        -- if empty, move pusher to next position
        Just Empty -> (next, Map.insert next p (Map.insert pusher Empty input))
        Just Box ->
          -- try propagating the push
          let (next', newMap) = push (Box, next) input dir
              -- put empty where box was, and box where it's going
              updated = Map.insert next p (Map.insert pusher Empty newMap)
           in -- if the box can't move, don't move the pusher
              if next' == next
                then (pusher, input)
                -- otherwise move pusher forward and fix map
                else (next, updated)

gps :: Coordinate -> Int
gps (x, y) = (100 * y) + x

part1 :: Input -> OutputA
part1 (input, robot, directions) =
  let boxes =
        Map.keys
          . Map.filter (== Box)
          . snd
          . foldl (\(r, i) d -> push (Robot, r) i d) (robot, input)
          $ directions
   in sum $ map gps boxes

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
