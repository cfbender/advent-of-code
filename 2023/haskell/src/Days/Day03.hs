module Days.Day03 where

-- Day 3 in the books, and the syntax is starting to feel more comfy.
-- I maybe could've done more with parsing here but I think just getting
-- them as coordinates is fine. I spent too much time trying to make that fancier.
-- REALLY liking this language when it flows well for me right now.
-- I feel like the solutions you get can be so elegant (though mine are not)
-- but I'm not sure how readable any of this is to anyone else

import Data.Attoparsec.Text
import Data.Char
import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers
import Util.Util (neighbors)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser pure 0

------------ TYPES ------------
type Input = Map (Int, Int) Char

type OutputA = Int

type OutputB = Int

------------ PART A ------------
checkPart x
  | isDigit x = False
  | x == '.' = False
  | otherwise = True

isPart :: Input -> [(Int, Int)] -> Bool
isPart m = any (any (\s -> checkPart $ Map.findWithDefault '.' s m) . neighbors)

readPartNumber :: Input -> [(Int, Int)] -> Int
readPartNumber m l = read $ map (\s -> Map.findWithDefault '0' s m) $ reverse l

groupDigits :: Input -> [[(Int, Int)]]
groupDigits = concatMap groupRow . groupByY . Map.toList
  where
    -- group all rows in map back to a list of (yValue, [row list])
    groupByY list = Map.toList $ Map.fromListWith (++) [(y, [((x, y), v)]) | ((x, y), v) <- list]
    -- group adjacent digits in row
    groupRow (_, row) =
      filter (not . null)
        . map (map fst . filter (isDigit . snd))
        $ groupBy ((&&) `on` isDigit . snd) row

part1 :: Input -> OutputA
part1 m = sum . map (readPartNumber m) . filter (isPart m) $ groupDigits m

------------ PART B ------------
part2 :: Input -> OutputB
part2 m =
  sum
    . map (product . map snd)
    . filter (\l -> length l == 2)
    . map (gearParts . neighbors . fst)
    $ gears
  where
    -- list of ([coord list], part number)
    partNumbers = map (\l -> (l, readPartNumber m l)) . filter (isPart m) $ groupDigits m
    -- list of all gears coords
    gears = filter ((== '*') . snd) . Map.toList $ m
    -- helper fn to reject parts that aren't a subset of a gears neighbors
    gearParts gearNeighbors =
      filter
        ( \p ->
            not $ null (gearNeighbors `intersect` fst p)
        )
        partNumbers
