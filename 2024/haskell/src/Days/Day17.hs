module Days.Day17 where

-- just doing part 1 for now, the part 2 is likely some sort of memoization or something?
-- or probably just reverse engineering the program to see what a register would give an expected output
-- idk I don't really feel like doing it quite yet. part 1 was so fun tho!! maybe I just
-- do part 1s

import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import Data.Bits (xor)
import Data.List (find, intercalate, unfoldr)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Util (third, traceShowIdWithContext)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  a <- "Register A: " *> decimal <* endOfLine
  b <- "Register B: " *> decimal <* endOfLine
  c <- "Register C: " *> decimal <* endOfLine
  endOfLine
  instructions <- "Program: " *> decimal `sepBy` ","
  pure (Registers a b c, V.fromList instructions)

------------ TYPES ------------
type Input = (Registers, Vector Int)

type OutputA = String

type OutputB = Int

data Registers = Registers
  { a :: Int,
    b :: Int,
    c :: Int
  }
  deriving (Show)

------------ PART 1 ------------
combo :: Registers -> Int -> Int
combo (Registers a b c) x
  | x <= 3 = x
  | x == 4 = a
  | x == 5 = b
  | x == 6 = c
  | x == 7 = error "invalid"

op :: (Int, Registers, [Int]) -> (Int, Int) -> (Int, Registers, [Int])
op (i, Registers a b c, out) (0, x) = (i + 2, Registers (a `div` (2 ^ x)) b c, out) -- adv
op (i, Registers a b c, out) (1, x) = (i + 2, Registers a (xor b x) c, out) -- bxl
op (i, Registers a b c, out) (2, x) = (i + 2, Registers a (x `mod` 8) c, out) -- bst
op (i, Registers a b c, out) (3, x) -- jnz
  | a == 0 = (i + 2, Registers a b c, out)
  | otherwise = (x, Registers a b c, out)
op (i, Registers a b c, out) (4, _) = (i + 2, Registers a (xor b c) c, out) -- bxc
op (i, Registers a b c, out) (5, x) = (i + 2, Registers a b c, (x `mod` 8) : out) -- out
op (i, Registers a b c, out) (6, x) = (i + 2, Registers a (a `div` (2 ^ x)) c, out) -- bdv
op (i, Registers a b c, out) (7, x) = (i + 2, Registers a b (a `div` (2 ^ x)), out) -- cdv

run' :: Vector Int -> (Int, Registers, [Int]) -> Maybe (Int, Registers, [Int])
run' instructions (i, regs, out) = case (instructions V.!? i, instructions V.!? (i + 1)) of
  (Just x, Just y) ->
    let operand = if x == 1 || x == 3 then y else combo regs y -- only use combo operand for bxl and jnz
     in Just (op (i, regs, out) (x, operand))
  _ -> Nothing

run :: Vector Int -> (Int, Registers, [Int]) -> String
run instructions state =
  intercalate ","
    . map show
    . reverse
    . third
    . last
    $ unfoldr step state
  where
    step state = case run' instructions state of
      Just newState -> Just (state, newState)
      Nothing -> Nothing

part1 :: Input -> OutputA
part1 (regs, instructions) = run instructions (0, regs, [])

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 (regs, instructions) =
  fst
    . fromJust
    . find ((== correct) . snd)
    $ [(x, run instructions (0, regs {a = x}, [])) | x <- [0 ..]]
  where
    correct = traceShowIdWithContext "correct" $ intercalate "," $ map show $ V.toList instructions
