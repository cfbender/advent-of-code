module Days.Day25 where


import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine)
import Data.Void


runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
part1 :: Input -> OutputA
part1 = error "Not implemented yet!"

------------ PART B ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
