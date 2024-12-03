module Days.Day03 where

import Data.Attoparsec.Text (Parser, endOfLine, sepBy, takeText)
import Data.Text qualified as T
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Regex.TDFA

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = T.unpack . T.strip <$> takeText

-- maybe I can make this work eventually
-- inputParser = filter (/= (0,0)) <$> many' (choice
--   [ do
--       string "mul("
--       x <- decimal
--       char ','
--       y <- decimal
--       char ')'
--       return (x, y)
--   , anyChar >> return (0,0)
--   ])

------------ TYPES ------------
type Input = String

type OutputA = [String]

type OutputB = Void

------------ PART 1 ------------
mulRegex :: String
mulRegex = "mul\\(([[:digit:]]+),([[:digit:]]+)\\)"

part1 :: Input -> OutputA
part1 input = getAllTextMatches (input =~ mulRegex) :: [String]

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
