module Days.Day21 where

import Data.Attoparsec.Text (Parser, anyChar, char, choice, digit, endOfLine, letter, many1, sepBy)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instructions `sepBy` endOfLine
 where
  instructions = do
    instructions <- many1 (choice [digit, char 'A'])
    return (map instruction instructions)
  instruction '0' = Zero
  instruction '1' = One
  instruction '2' = Two
  instruction '3' = Three
  instruction '4' = Four
  instruction '5' = Five
  instruction '6' = Six
  instruction '7' = Seven
  instruction '8' = Eight
  instruction '9' = Nine
  instruction 'A' = Push

------------ TYPES ------------
type Input = [[NumInstruction]]

type OutputA = String

type OutputB = Void

data NumInstruction
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Push
  deriving (Show, Eq)

------------ PART 1 ------------
part1 :: Input -> OutputA
part1 = show

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
