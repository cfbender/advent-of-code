module Days.Day24 where

-- crazy part 2, don't feel like dealing with having to check 2^46 numbers.
-- obviously some trick, don't wanna figure it out
-- pretty happy with part 1 tho! even though it's a little late. fun plane activity

import Data.Attoparsec.Text (Parser, anyChar, char, choice, count, decimal, endOfLine, many1, sepBy, string)
import Data.Bits (xor, (.&.), (.|.))
import Data.List (isPrefixOf, sort, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  states <- state `sepBy` endOfLine
  count 2 endOfLine
  gates <- gate `sepBy` endOfLine
  return (Map.fromList states, gates)
  where
    state = do
      name <- count 3 anyChar
      string ": "
      value <- decimal
      return (name, value)
    gate = do
      first <- count 3 anyChar
      char ' '
      gateType <- gateType' <$> choice [string "AND", string "OR", string "XOR"]
      char ' '
      second <- count 3 anyChar
      string " -> "
      output <- count 3 anyChar
      return (Gate (first, second) output gateType)
    gateType' "AND" = AND
    gateType' "OR" = OR
    gateType' "XOR" = XOR

------------ TYPES ------------
type Input = (Map String Int, [Gate])

type OutputA = Int

type OutputB = Void

data Operation = AND | OR | XOR deriving (Show, Eq)

data Gate = Gate
  { inputs :: (String, String),
    output :: String,
    operation :: Operation
  }
  deriving (Show, Eq)

------------ PART 1 ------------
getOutput :: Map String Int -> Map String Gate -> String -> Int
getOutput state gates wire = case Map.lookup wire state of
  Just value -> value
  Nothing -> case Map.lookup wire gates of
    Just gate -> case gate of
      Gate (a, b) _ AND -> getOutput state gates a .&. getOutput state gates b
      Gate (a, b) _ OR -> getOutput state gates a .|. getOutput state gates b
      Gate (a, b) _ XOR -> getOutput state gates a `xor` getOutput state gates b
    Nothing -> error $ "Could not find wire " ++ wire

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs

part1 :: Input -> OutputA
part1 (state, gates) = convert $ map (getOutput state byOutput) digitWires
  where
    byOutput = Map.fromList $ map (\g -> (output g, g)) gates
    digitWires = sort . filter ((== 'z') . head) $ map output gates

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 (state, gates) = error "Not implemented yet!"
