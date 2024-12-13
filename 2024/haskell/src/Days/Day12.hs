module Days.Day12 where

-- hardest one yet for me for sure
-- first I way overcomplicated the perimeter by walking the edges
-- and that worked but then I stumbled onto the "4 - length connected" approach I have here now
--
-- had to look for hints on how to handle the edges for part 2
-- and made that into this liminal points solution.
-- which we have definitely done something like that before in AoC, maybe last year with the pipes or something?
-- Still don't have habits for polygons that would've let me short circuit here
--
-- otherwise pretty happy with what I ended up with, just not happy with the speed it took me to get there.
--
-- day 13 already out but saving that for tomorrow (today)

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates (Direction (E, N, S, W), neighborsNoCorners, neighborsOnlyCorners)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser Just 0

------------ TYPES ------------
type Input = Map Coordinate Char

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
connected :: Input -> Coordinate -> [Coordinate]
connected input coord = filter (\n -> Just match == input M.!? n) $ neighborsNoCorners coord
  where
    match = input M.! coord

allConnected :: Input -> Coordinate -> [Coordinate]
allConnected input coord = S.toList $ go [coord] S.empty
  where
    match = input M.! coord
    go :: [Coordinate] -> Set Coordinate -> Set Coordinate
    go [] seen = seen
    go (coord : coords) seen
      | coord `S.member` seen = go coords seen
      | input M.!? coord == Just match = go (connected input coord ++ coords) (S.insert coord seen)
      | otherwise = go coords seen

regions :: Input -> Map Char [[Coordinate]]
regions input = M.map (map S.toList) $ fst $ foldr step (M.empty, S.empty) $ M.keys input
  where
    -- iterate through input check if point already added
    -- if not, add all connected points to a region and add all to seen
    step :: Coordinate -> (Map Char [Set Coordinate], Set Coordinate) -> (Map Char [Set Coordinate], Set Coordinate)
    step coord (regions, seen)
      | S.member coord seen = (regions, seen)
      | otherwise = (newRegions, newSeen)
      where
        newRegions = M.insertWith (++) name [connected] regions
        newSeen = seen `S.union` connected
        connected = S.fromList $ allConnected input coord
        name = input M.! coord

perimeter :: Input -> [Coordinate] -> Int
perimeter input xs = sum $ map ((4 -) . length . connected input) xs

area :: [Coordinate] -> Int
area = length

price :: (Input -> [Coordinate] -> Int) -> Input -> Int
price mapper input =
  sum
    . map (\list -> mapper input list * area list)
    $ concatMap snd
    $ M.toList
    $ regions input

part1 :: Input -> OutputA
part1 = price perimeter

------------ PART 2 ------------
corners :: Coordinate -> [(Double, Double)]
corners (x, y) =
  [ (x' - 0.5, y' - 0.5),
    (x' - 0.5, y' + 0.5),
    (x' + 0.5, y' - 0.5),
    (x' + 0.5, y' + 0.5)
  ]
  where
    x' = fromIntegral x
    y' = fromIntegral y

uncorner :: (Double, Double) -> [Coordinate]
uncorner (x, y) =
  [ (floor x, floor y),
    (ceiling x, floor y),
    (floor x, ceiling y),
    (ceiling x, ceiling y)
  ]

isTouchingCorner :: Coordinate -> Coordinate -> Bool
isTouchingCorner (x1, y1) (x2, y2) = abs (x1 - x2) == 1 && abs (y1 - y2) == 1

edges :: Input -> [Coordinate] -> Map (Double, Double) Int
edges input xs = U.freq (edgeCorners ++ mapCorners)
  where
    mapCorners = concatMap corners xs
    check point =
      -- get surrounding real points and check which of those are in this region and touching corners
      let surrounding = filter (`elem` xs) $ uncorner point
       in (== 2) . length $
            filter (\p -> any (isTouchingCorner p) surrounding) surrounding
    edgeCorners =
      -- hack to make the corner count as two corners
      concatMap (\(x, y) -> [(x, y), (x + 0.1, y + 0.1)])
        -- find all keys where the check function produces two points that both
        -- claim a single liminal point
        . M.keys
        $ M.filter (== 2)
        $ U.freq
        $ filter check mapCorners

-- one time means it's a corner
-- two times means an edge
-- three times means corner
-- four times means internal
freqs :: Input -> [Coordinate] -> Int
freqs i t = length . filter odd $ M.elems $ edges i t

-- 912944 too low
part2 :: Input -> OutputB
part2 = price freqs
