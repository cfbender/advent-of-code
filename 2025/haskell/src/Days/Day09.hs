module Days.Day09 where

-- part 1 ezpz, nice and straightforward.
-- part 2 I knew would be some even-odd rule thing, and implemented that but it was slow
-- and incorrect. got some help from claude to point me in the right direction checking the
-- polygon edges against the rectangle interiors, which got the answer.
-- also parallelized the rectangle checking to speed it up.

import Control.Parallel.Strategies
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, around)
import Util.Util (safeHead)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinate `sepBy` endOfLine
  where
    coordinate = decimal `around` char ','

------------ TYPES ------------
type Input = [Coordinate]

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
area (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = [(x, y) | y <- xs] ++ allPairs xs

part1 :: Input -> OutputA
part1 = maximum . map (uncurry area) . allPairs

------------ PART 2 ------------
between :: (Ord a) => a -> a -> a -> Bool
between x a b = (min a b < x) && (x < max a b)

isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 1

isInside :: [(Coordinate, Coordinate)] -> Coordinate -> Bool
isInside edges (x, y) =
    isOdd
        . length
        $ filter crossesRay edges
  where
    crossesRay ((x', y'), (x'', y''))
        | x' /= x'' = False -- horizontal edge
        | x' < x = False -- left of ray
        | otherwise = between y y' y''

isInsideOrOnBoundary :: [(Coordinate, Coordinate)] -> Coordinate -> Bool
isInsideOrOnBoundary edges pt =
    isOnBoundary edges pt || isInside edges pt
  where
    isOnBoundary edges (x, y) = any (onEdge (x, y)) edges

    onEdge (x, y) ((x1, y1), (x2, y2))
        | x1 == x2 && x == x1 = (min y1 y2 <= y) && (y <= max y1 y2) -- On vertical edge
        | y1 == y2 && y == y1 = (min x1 x2 <= x) && (x <= max x1 x2) -- On horizontal edge
        | otherwise = False

oppositeCorners :: (Coordinate, Coordinate) -> (Coordinate, Coordinate)
oppositeCorners ((x1, y1), (x2, y2)) = ((x1, y2), (x2, y1))

part2 :: Input -> OutputB
part2 redTiles = maximum . map (uncurry area) $ validRects
  where
    polygonEdges = zip redTiles (drop 1 redTiles ++ [safeHead redTiles])
    allRectangles = [(redCorners, oppositeCorners redCorners) | redCorners <- allPairs redTiles]
    -- A rectangle is valid if its non-red corners are inside the polygon
    -- AND no polygon edges cut through the rectangle interior
    isValidRect (redCorners@(r1, r2), otherCorners@(c1, c2)) =
        cornersInsidePolygon && noEdgesCrossInterior
      where
        (minX, maxX) = (min (fst r1) (fst r2), max (fst r1) (fst r2))
        (minY, maxY) = (min (snd r1) (snd r2), max (snd r1) (snd r2))

        cornersInsidePolygon = all (isInsideOrOnBoundary polygonEdges) [c1, c2]

        -- Only check edges not connected to our red corners (those will naturally touch)
        unrelatedEdges = filter (\(p1, p2) -> p1 /= r1 && p1 /= r2 && p2 /= r1 && p2 /= r2) polygonEdges

        edgeCrossesInterior ((x1, y1), (x2, y2))
            | x1 == x2 -- Vertical edge at x
                =
                let x = x1
                    (yMin, yMax) = (min y1 y2, max y1 y2)
                 in (minX < x && x < maxX) && (max yMin minY < min yMax maxY)
            | y1 == y2 -- Horizontal edge at y
                =
                let y = y1
                    (xMin, xMax) = (min x1 x2, max x1 x2)
                 in (minY < y && y < maxY) && (max xMin minX < min xMax maxX)
            | otherwise = False
        noEdgesCrossInterior = not $ any edgeCrossesInterior unrelatedEdges
    validRects = map snd (filter isValidRect allRectangles `using` parList rdeepseq)
