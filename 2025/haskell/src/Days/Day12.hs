module Days.Day12 where

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART 1 ------------
part1 :: Input -> OutputA
part1 = error "Not implemented yet!"

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
