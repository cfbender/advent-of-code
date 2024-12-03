module Days.Day03 where

-- ah yikes. I spent forever on getting distracted on project setup fixes from last year.
-- I ended up updating the template and pushing that up which is nice, but I need to document it some.
-- anyway...
-- This was a cool problem. I was stupid and missed the 1-3 digit requirement which was messing
-- up my solution for quite a while. I tried regex to fix it but regex isn't famous for fixing as much as breaking.
-- Happy that I got a parser combinator solution out of it, but still a lot of remembering to do when it comes to Haskell this year

import Data.Attoparsec.Text
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Regex.TDFA

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  filter (/= Mul (0, 0))
    <$> many'
      ( choice
          [ string "don't()" >> return Dont
          , string "do()" >> return Do
          , do
              string "mul("
              x <- number
              char ','
              y <- number
              char ')'
              return (Mul (x, y))
          , anyChar >> return (Mul (0, 0))
          ]
      )
 where
  -- limit to 1-3 digits
  number = read <$> choice [count 3 digit, count 2 digit, count 1 digit]

------------ TYPES ------------
type Input = [Instruction]

type OutputA = Int

type OutputB = Int

data Instruction = Mul (Int, Int) | Dont | Do deriving (Show, Eq)

------------ PART 1 ------------
part1 :: Input -> OutputA
convert (Mul (x, y)) = x * y
convert Dont = 0
convert Do = 0

part1 = sum . map convert

------------ PART 2 ------------
part2 :: Input -> OutputB
result :: (Instruction, Int) -> Instruction -> (Instruction, Int)
-- set to don't regardless of instruction
result (_, x) Dont = (Dont, x)
-- set to do
result (_, x) Do = (Do, x)
-- only multiply if Do was set
result (Do, z) (Mul (x, y)) = (Do, z + (x * y))
-- skip if don't was set
result (Dont, z) _ = (Dont, z)
part2 = snd . foldl result (Do, 0)
