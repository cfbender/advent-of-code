module Days.Day08 where

-- was pretty stuck on part 1 for this one, and had to ask claude for a hint
-- which pointed me to Kruska's algorithm. That was a new one for me, and pretty fun!

import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.List (sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..), comparing)
import Program.RunDay qualified as R (Day, runDay)
import Util.Util (third)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = pointParser `sepBy` endOfLine
  where
    pointParser = do
        x <- decimal
        _ <- char ','
        y <- decimal
        _ <- char ','
        z <- decimal
        return (x, y, z)

------------ TYPES ------------
type Input = [Point]

type OutputA = Int

type OutputB = Int

type Point = (Int, Int, Int)
type UnionFind = Map Point Point

------------ PART 1 ------------
distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2))

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = [(x, y) | y <- xs] ++ allPairs xs

find :: UnionFind -> Point -> (Point, UnionFind)
find uf p =
    let parent = uf M.! p
     in if parent == p
            then (p, uf)
            else
                let (root, uf') = find uf parent
                 in (root, M.insert p root uf')

union :: UnionFind -> Point -> Point -> (Bool, UnionFind)
union uf p1 p2 =
    let (root1, uf') = find uf p1
        (root2, uf'') = find uf' p2
     in if root1 == root2
            then (False, uf'') -- already in same set
            else (True, M.insert root1 root2 uf'') -- connect them

kruskal :: Int -> UnionFind -> [(Point, Point, Double)] -> UnionFind
kruskal 0 uf _ = uf
kruskal _ uf [] = uf
kruskal count uf ((p1, p2, _) : rest) =
    let (_, uf') = union uf p1 p2
     in kruskal (count - 1) uf' rest

-- Get component sizes
componentSizes :: UnionFind -> [Int]
componentSizes uf =
    let points = M.keys uf
        roots = map (fst . find uf) points
        counts = M.fromListWith (+) [(r, 1) | r <- roots]
     in M.elems counts

solve :: Int -> Input -> OutputA
solve numConnections points = product $ take 3 $ sortOn Down sizes
  where
    pairs = [(p1, p2, distance p1 p2) | (p1, p2) <- allPairs points]
    sortedPairs = sortBy (comparing third) pairs
    uf = M.fromList [(p, p) | p <- points]
    finalUf = kruskal numConnections uf sortedPairs
    sizes = componentSizes finalUf

part1 :: Input -> OutputA
part1 = solve 1000

------------ PART 2 ------------

kruskal' :: UnionFind -> [(Point, Point, Double)] -> Maybe (Point, Point) -> (UnionFind, Maybe (Point, Point))
kruskal' uf [] l = (uf, l)
kruskal' uf ((p1, p2, _) : rest) l =
    let (_, uf') = union uf p1 p2
     in if length (componentSizes uf') == 1
            then (uf', Just (p1, p2))
            else kruskal' uf' rest (Just (p1, p2))

part2 :: Input -> OutputB
part2 i = x1 * x2
  where
    pairs = [(p1, p2, distance p1 p2) | (p1, p2) <- allPairs i]
    sortedPairs = sortBy (comparing third) pairs
    uf = M.fromList [(p, p) | p <- i]
    (finalUf, Just ((x1, _, _), (x2, _, _))) = kruskal' uf sortedPairs Nothing
    sizes = componentSizes finalUf
