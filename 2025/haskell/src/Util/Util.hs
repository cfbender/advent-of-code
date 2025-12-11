module Util.Util where

import Control.Applicative
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
mapFromNestedLists :: (Ord a) => [[a]] -> Map Coordinate a
mapFromNestedLists = Map.fromList . attachCoords 0 0

attachCoords :: Int -> Int -> [[a]] -> [(Coordinate, a)]
attachCoords _ _ [] = []
attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

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

-- Splits a list into two lists
-- where those that satisfy the predicate are on the left
-- and otherwise on the right
splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith p =
    foldr
        ( \x (a, b) ->
            if p x
                then (x : a, b)
                else (a, x : b)
        )
        ([], [])

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

groupBy :: (Ord a) => (t -> a) -> [t] -> Map a [t]
groupBy mapper input = Map.fromListWith (++) (map (\x -> (mapper x, [x])) input)

-- split an integer into it's digits
digits :: Int -> [Int]
digits = map (read . (: [])) . show

-- join a list of integer digits into a single integer
undigits :: [Int] -> Int
undigits = read . concatMap show

uniq :: (Ord a) => [a] -> [a]
uniq = Set.toList . Set.fromList

tsnd (_, x, _) = x

third (_, _, x) = x

safeHead :: [a] -> a
safeHead [] = error "safeHead: empty list"
safeHead (x : _) = x
