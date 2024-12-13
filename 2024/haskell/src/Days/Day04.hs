module Days.Day04 where

-- Fun little one! Not really my most readable code, but I'm happy with the solution.
-- was accidentally counting XSAM for a while because I thought I needed to consider reversed
-- but since starting with X and getting rays in order there's
-- no need (since one of the rays would check SAMX, etc.)
--
-- Feels like I'm finding the groove again which feels good,
-- but still not super comfy with haskell as a tool yet

import Data.Attoparsec.Text (Parser, char, choice, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, coordinateParser)
import Util.Util (neighborsOnlyCorners, rays)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser xmas 0
  where
    xmas 'X' = Just X
    xmas 'M' = Just M
    xmas 'A' = Just A
    xmas 'S' = Just S
    xmas _ = Nothing

------------ TYPES ------------
type Input = Map Coordinate XMAS

type OutputA = Int

type OutputB = Int

data XMAS = X | M | A | S deriving (Show, Eq)

------------ PART 1 ------------
part1 :: Input -> OutputA
found :: [XMAS] -> Bool
-- since we're only checking Xs, rays that count should just equal [M, A, S]
found (M : A : S : _) = True
found _ = False
part1 input =
  length . filter found $
    -- flatmap the checks for each X into a list of lists of letters
    checks
      >>= map
        -- convert coordinates from rays to letters
        (mapMaybe (input M.!?))
  where
    xs = filter ((==) X . snd) $ M.toList input
    -- get rays coming from Xs in order
    checks = map (rays 3 . fst) xs

------------ PART 2 ------------
part2 :: Input -> OutputB
found' :: [XMAS] -> Bool
-- neighborsOnlyCorners returns [bottomLeft, topLeft, bottomRight, topRight]]
-- so count all rotations
found' (S : S : M : M : _) = True
found' (M : S : M : S : _) = True
found' (M : M : S : S : _) = True
found' (S : M : S : M : _) = True
found' _ = False
part2 input = length . filter found' $ map (mapMaybe (input M.!?)) checks
  where
    as = filter ((==) A . snd) $ M.toList input
    checks = map (neighborsOnlyCorners . fst) as
