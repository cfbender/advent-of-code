module Util.Coordinates where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Util.Parsers (Coordinate)

data Direction = N | E | S | W | NE | SE | SW | NW deriving (Show, Eq, Ord)

-- move in a given direction
move :: Coordinate -> Direction -> Coordinate
move (x, y) N = (x, y - 1)
move (x, y) E = (x + 1, y)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)
move (x, y) NE = (x + 1, y - 1)
move (x, y) SE = (x + 1, y + 1)
move (x, y) SW = (x - 1, y + 1)
move (x, y) NW = (x - 1, y - 1)

-- gets all neighbors around a coordinate
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (a, b)
    | a <- [x - 1 .. x + 1],
      b <- [y - 1 .. y + 1],
      a /= x || b /= y
  ]

-- gets neighbors in all directions and continues those
-- for a specified number of steps
rays :: Int -> Coordinate -> [[Coordinate]]
rays z (x, y)
  | z < 1 = []
  | otherwise =
      -- left
      [ reverse [(a, y) | a <- [x - z .. x - 1]],
        -- right
        [(a, y) | a <- [x + 1 .. x + z]],
        -- down
        reverse [(x, b) | b <- [y - z .. y - 1]],
        -- up
        [(x, b) | b <- [y + 1 .. y + z]],
        -- down-left
        reverse [(a, b) | a <- [x - z .. x - 1], b <- [y - z .. y - 1], (b - y) == (a - x)],
        -- down-right
        [(a, b) | a <- [x + 1 .. x + z], b <- [y - z .. y - 1], (y - b) == (a - x)],
        -- up-left
        reverse [(a, b) | a <- [x - z .. x - 1], b <- [y + 1 .. y + z], (y - b) == (a - x)],
        -- up-right
        [(a, b) | a <- [x + 1 .. x + z], b <- [y + 1 .. y + z], (b - y) == (a - x)]
      ]

-- gets all neighbors around a coordinate
neighborsNoCorners :: (Int, Int) -> [(Int, Int)]
neighborsNoCorners (x, y) =
  [ (x - 1, y),
    (x + 1, y),
    (x, y - 1),
    (x, y + 1)
  ]

-- gets all neighbors around a coordinate
neighborsNoCornersSet :: (Int, Int) -> Set (Int, Int)
neighborsNoCornersSet = Set.fromList . neighborsNoCorners

neighborsOnlyCorners :: (Int, Int) -> [(Int, Int)]
neighborsOnlyCorners (x, y) =
  [ (x - 1, y - 1),
    (x - 1, y + 1),
    (x + 1, y - 1),
    (x + 1, y + 1)
  ]

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
type Bounds = (Int, Int, Int, Int)

mapBoundingBox :: Map (Int, Int) a -> Bounds
mapBoundingBox m =
  (,,,)
    (minimum . fmap fst . Map.keys $ m)
    (maximum . fmap fst . Map.keys $ m)
    (minimum . fmap snd . Map.keys $ m)
    (maximum . fmap snd . Map.keys $ m)

setBoundingBox :: Set Coordinate -> Bounds
setBoundingBox m =
  (,,,)
    (minimum . fmap fst . Set.toList $ m)
    (maximum . fmap fst . Set.toList $ m)
    (minimum . fmap snd . Set.toList $ m)
    (maximum . fmap snd . Set.toList $ m)

dijkstras :: Coordinate -> Coordinate -> (Int -> a -> a -> Int) -> Map Coordinate a -> Int
dijkstras start end mapper i = dijkstras' (Set.singleton (0, start)) Map.empty
  where
    isCheaper m (cost, p) = not (Map.member p m) || (cost < m Map.! p)
    dijkstras' candidates costs =
      let (c@(cost, curr), rest) = Set.deleteFindMin candidates
          candidates' =
            filter (isCheaper costs)
              . map (\loc -> (mapper cost (i Map.! curr) (i Map.! loc), loc))
              . filter (`Map.member` i)
              $ neighborsNoCorners curr
          newCandidates = Set.union (Set.fromList candidates') rest
          candidateCosts = map (\(cost, coord) -> (coord, cost)) candidates'
          newCosts = foldr (uncurry Map.insert) costs candidateCosts
       in if curr == end
            then cost
            else dijkstras' newCandidates newCosts

printMap :: Map Coordinate a -> (Maybe a -> String) -> IO (Map Coordinate a)
printMap m display = do
  mapM_ putStrLn lines
  return m
  where
    lines = map (concatMap display . line) [minY .. maxY]
    (minX, maxX, minY, maxY) = mapBoundingBox m
    line y = [m Map.!? (x, y) | x <- [minX .. maxX]]

toPrintable :: Map Coordinate a -> (Maybe a -> String) -> String
toPrintable m display = intercalate "\n" lines
  where
    lines = map (concatMap display . line) [minY .. maxY]
    (minX, maxX, minY, maxY) = mapBoundingBox m
    line y = [m Map.!? (x, y) | x <- [minX .. maxX]]
