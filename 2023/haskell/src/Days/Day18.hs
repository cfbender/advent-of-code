module Days.Day18 where

-- tfw you're up at 3AM googling "area of a complex polyhedron"
-- but you really meant to google "area of a polygon"
-- and you don't find shoelace theorem until someone tells you about it
--
--
-- who's laughing now, idiots.

import Data.Attoparsec.Text
import Numeric (readHex)
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate)
import Util.Util (windows)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where
    instruction = do
      direction <- choice [char 'U', char 'D', char 'L', char 'R']
      space
      num <- decimal
      space
      string "(#"
      [(code, _)] <- readHex <$> count 5 anyChar
      realDir <- count 1 anyChar
      char ')'
      return (direction, num, (real realDir, code))
    real "0" = 'R'
    real "1" = 'D'
    real "2" = 'L'
    real "3" = 'U'

------------ TYPES ------------
type Instruction = (Char, Int, (Char, Int))

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
dig :: (Int, [Coordinate]) -> Coordinate -> Instruction -> ((Int, [Coordinate]), Coordinate)
dig (perim, cs) (x, y) (dir, count, _) = ((perim + count, new : cs), new)
  where
    new = coord dir
    coord 'U' = (x, y - count)
    coord 'D' = (x, y + count)
    coord 'L' = (x - count, y)
    coord 'R' = (x + count, y)

area :: Int -> [Coordinate] -> Int
area perim s =
  floor
    . ((+ 1) . (+ (fromIntegral perim / 2)) . (/ 2) . fromIntegral)
    . sum
    $ [(x * y') - (x' * y) | ((x, y) : (x', y') : ys) <- windows 2 $ reverse s]

part1 :: Input -> OutputA
part1 i = area perim points
  where
    ((perim, points), _) = foldl (uncurry dig) ((0, [(0, 0)]), (0, 0)) i

------------ PART B ------------
part2 :: Input -> OutputB
part2 i = area perim points
  where
    actual = map (\(_, _, d@(dir, count)) -> (dir, count, d)) i
    ((perim, points), _) = foldl (uncurry dig) ((0, [(0, 0)]), (0, 0)) actual
