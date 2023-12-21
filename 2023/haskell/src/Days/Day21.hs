module Days.Day21 where

-- really disliked this
-- fun enough but just BFS part one,
-- but then another part 2 where not only is there a math trick
-- (be that geometry or linear algebra)
-- BUT you can't even properly test your input!!
--
-- this isn't what I do AoC for, but I'm glad at least some people enjoy this.

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (neighborsNoCornersSet)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  coords <- coordinateParser garden 0
  return (result coords)
  where
    garden '.' = Just Empty
    garden '#' = Just Rock
    garden 'S' = Just Start
    result c =
      let (Just (start, _)) = find ((== Start) . snd) $ M.toList c
       in (start, M.insert start Empty c)

------------ TYPES ------------
data Location = Start | Rock | Empty deriving (Eq, Show)

type Input = (Coordinate, Map Coordinate Location)

type OutputA = Int

type OutputB = String

------------ PART A ------------
search :: Int -> Set Coordinate -> (Coordinate -> Bool) -> Set Coordinate
search 0 l _ = l
search n l f = search (n - 1) (S.filter f next) f
  where
    -- get all neighbors as set
    next = S.foldr add S.empty l
    add c acc = neighborsNoCornersSet c `S.union` acc

part1 :: Input -> OutputA
part1 (start, m) = S.size $ search 64 (S.singleton start) isAvailable
  where
    isAvailable c =
      let point = m M.!? c
       in point == Just Empty || isNothing point

------------ PART B ------------
part2 :: Input -> OutputB
part2 (start, m) = show (y 26501365)
  where
    -- size of input is 131
    -- 65 steps to get to the next spot in the same garden
    -- 260501365 mod 131 = 65
    -- so is periodic over 202300 cycles
    -- but for some reason mine gives the right input at 26501365
    -- instead of 202300. who cares I'm leaving it
    (_, xMax, _, _) = U.mapBoundingBox m
    size = xMax + 1
    travel = floor $ fromIntegral size / 2
    trials = [travel, travel + size, travel + size * 2]
    [(x0, y0), (x1, y1), (x2, y2)] =
      map
        ( \x ->
            ( fromIntegral x,
              fromIntegral $ S.size $ search x (S.singleton start) isAvailable
            )
        )
        trials
    y :: Double -> Int
    y x =
      ceiling $
        -- https://en.wikipedia.org/wiki/Lagrange_polynomial
        sum
          [ y0 * ((x - x1) / (x0 - x1)) * ((x - x2) / (x0 - x2)),
            y1 * ((x - x0) / (x1 - x0)) * ((x - x2) / (x1 - x2)),
            y2 * ((x - x0) / (x2 - x0)) * ((x - x1) / (x2 - x1))
          ]
    isAvailable (x, y) =
      let clamped =
            ( mod (x + xMax + 1) (xMax + 1),
              mod (y + xMax + 1) (xMax + 1)
            )
          point = m M.!? clamped
       in point == Just Empty || isNothing point
