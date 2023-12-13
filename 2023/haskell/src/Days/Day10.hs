module Days.Day10 where

-- woof this one was tough for me.
-- still a lot to learn on haskell, but this was a good one for learning!
--
-- probably should've just done dijktstras from the start
-- instead of this weird version of it I came out with but oh well.
--
-- maybe I'll refactor to get a real dijkstras implementation out of it later.
--
-- learned about point-in-polygon problems too which is fun!
--
-- tough, kinda annoying, decent learning. 7/10

{- ORMOLU_DISABLE -}
import Data.List (find)
import Data.Map (Map, insert, member)
import qualified Data.Map as Map
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser)
import Util.Parsers (coordinateParser)
import Data.Maybe (mapMaybe)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser pipe 0
  where
    pipe '|' = Just Vertical
    pipe '-' = Just Horizontal
    pipe 'L' = Just BendNorthEast
    pipe 'J' = Just BendNorthWest
    pipe '7' = Just BendSouthWest
    pipe 'F' = Just BendSouthEast
    pipe 'S' = Just Start
    pipe '.' = Just Empty

------------ TYPES ------------
data Pipe
  = Start
  | Empty
  | Vertical
  | Horizontal
  | BendNorthEast
  | BendNorthWest
  | BendSouthWest
  | BendSouthEast
  deriving (Show, Eq, Enum, Ord)

type Coordinate = (Int, Int)

type Location = (Coordinate, Pipe)

type Input = Map Coordinate Pipe

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- returns the start from the map  with it's pipe type
start :: Map (Int, Int) Pipe -> ((Int, Int), Pipe)
start m =
  let Just (coord, Start) = find (\(k, v) -> v == Start) $ Map.toList m
   in -- hard coded start pipe type. ain't nobody got time for that
      (coord, BendSouthWest)

getPoint :: Input -> (Int, Int) -> Maybe Location
getPoint m a = wrap (m Map.!? a)
  where
    wrap Nothing = Nothing
    wrap (Just x) = Just (a, x)

connected :: Input -> ((Int, Int), Pipe) -> [((Int, Int), Pipe)]
connected m ((x, y), t) = case t of
  Vertical -> points (x, y + 1) (x, y - 1)
  Horizontal -> points (x - 1, y) (x + 1, y)
  BendNorthEast -> points (x + 1, y) (x, y - 1)
  BendNorthWest -> points (x - 1, y) (x, y - 1)
  BendSouthWest -> points (x - 1, y) (x, y + 1)
  BendSouthEast -> points (x + 1, y) (x, y + 1)
  Start -> []
  Empty -> []
  where
    points a b = mapMaybe (getPoint m) [a, b]

-- kinda did dijsktras without doing it
crawl :: Input -> Int -> Map Location Int -> [Location] -> Map Location Int
crawl _ _ a [] = a
crawl pipes dist costs p = crawl pipes d' (foldr insert' costs next') next
  where
    insert' (p, d) = insert p d
    d' = dist + 1
    next =
      concatMap
        ( filter
            ( \k ->
                compare (costs Map.!? k) d'
                  && notElem k p
            )
            . connected pipes
        )
        p
    next' = map (,d') next
    compare Nothing v = True
    compare (Just m) v = m > v

part1 :: Input -> OutputA
part1 m = maximum . map snd . Map.toList $ crawl m' 0 (Map.fromList [(startPoint, 0)]) [startPoint]
  where
    startPoint = start m
    m' = uncurry insert startPoint m

------------ PART B ------------
-- https://en.wikipedia.org/wiki/Nonzero-rule
isCrossing p = case p of
  Horizontal -> False
  Empty -> False
  _ -> True

inside :: Input -> Map Location Int -> Int -> Coordinate -> Bool
inside pipes costs xBound p@(x, y) =
  not (onLoop p) && ((== 1) . abs $ foldr ((+) . deltaWind) 0 loopPoints)
  where
    point p = pipes Map.! p
    onLoop p = member (p, point p) costs
    -- look to the edge to the right
    ray = map (,y) [x + 1 .. xBound]
    -- only consider bends and verticals that are on the loop on the ray
    loopPoints = filter (\p -> isCrossing (point p) && onLoop p) ray

    -- find cost difference to determine direction of pipe
    diff :: Int -> Maybe Int -> Int
    diff a (Just b) = a - b
    diff a Nothing = 0
    -- maybe refactor to drop the pipe type from the costs map
    cost p = costs Map.! (p, point p)
    below (x, y) = costs Map.!? ((x, y + 1), point (x, y + 1))
    -- find wind delta from point and point below it
    deltaWind :: Coordinate -> Int
    deltaWind p =
      let diff' = diff (cost p) (below p)
       in if abs diff' == 1 then diff' else 0

part2 :: Input -> OutputB
part2 m = length . filter (inside m' loop xBound) $ Map.keys m'
  where
    loop = crawl m' 0 costs [startPoint]
    costs = Map.fromList [(startPoint, 0)]
    startPoint = start m
    m' = uncurry insert startPoint m
    xBound = maximum . fmap fst . Map.keys $ m
