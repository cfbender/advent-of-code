module Util.Util where

import Control.Applicative
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (sequenceA)
import Debug.Trace (trace)
import Util.Parsers (Coordinate)

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0

attachCoords :: Int -> Int -> [[a]] -> [((Int, Int), a)]
attachCoords _ _ [] = []
attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

-- gets all neighbors around a coordinate
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (a, b)
    | a <- [x - 1 .. x + 1],
      b <- [y - 1 .. y + 1],
      a /= x || b /= y
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

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | null ls = []
  | length ls < n = [ls]
  | otherwise = take n ls : chunksOf n (drop n ls)

-- Gets a sliding window in the list of the specified size.
-- Example:
--  windows 2 [1,2,3,4,5]
--    [[1,2], [2,3], [3,4], [4,5]]
windows :: Int -> [a] -> [[a]]
windows m = getZipList . traverse ZipList . take m . tails

-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
  | null ls = []
  | otherwise =
      let (prefix, rest) = span p ls
       in if null prefix
            then chunksByPredicate p $ dropWhile (not . p) rest
            else prefix : chunksByPredicate p (dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if
    | index < 0 -> Nothing
    | index >= length list -> Nothing
    | otherwise -> Just $ list !! index

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

tsnd (_, x, _) = x

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
    
freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . flip zip (repeat 1)
