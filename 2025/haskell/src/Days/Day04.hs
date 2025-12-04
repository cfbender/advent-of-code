module Days.Day04 where

-- easiest one yet! helps having the helpers from previous years to grab the neighbors
-- and that its very similar to problems in years past. loved it though, nice little warm-up for the day

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, endOfLine, many1, sepBy)
import Data.List (null)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinates (neighbors)
import Util.Parsers (Coordinate, coordinateParser)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser roll 0
  where
    roll '@' = Just Roll
    roll '.' = Nothing
    roll _ = Nothing

------------ TYPES ------------
data Position = Roll | Empty deriving (Show, Eq)

type Input = Map Coordinate Position

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
isAccessible :: Input -> Coordinate -> Bool
isAccessible input coord =
    let
        ns = neighbors coord
        mapped = mapMaybe (`M.lookup` input) ns
     in
        length mapped < 4

part1 :: Input -> OutputA
part1 input = length . filter (isAccessible input) $ M.keys input

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = removeAccessibleRolls 0
  where
    removeAccessibleRolls count input =
        let
            accessibleRolls = filter (isAccessible input) $ M.keys input
         in
            if null accessibleRolls
                then count
                else removeAccessibleRolls (count + length accessibleRolls) (foldr M.delete input accessibleRolls)
