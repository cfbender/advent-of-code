module Days.Day08 where

-- how do you, as a grown adult, re-learn math?
-- I mean seriously. how do I learn to think math-first?
-- it would've taken me a while to think of the LCM and idk why.
-- but thank goodness for hints.
--
-- anyway this was fun, more gold plating with the Graph, and I learned a lot
-- in the intermediate solutions so that's cool
--
-- okay edit: I refactored to remove the Graph. fun to learn, just made this
-- more confusing to read back later


import Data.Map ( Map)
import qualified Data.Map as Map
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void


runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------

inputParser :: Parser Input
inputParser = do
  instructions <- many1 (choice [char 'R', char 'L'])
  count 2 endOfLine
  mapLines <- mapLine `sepBy` endOfLine
  return (instructions, Map.fromList mapLines)
  where
    mapLine = do
      node <- count 3 letter
      string " = ("
      left <- count 3 letter
      string ", "
      right <- count 3 letter
      char ')'
      return (node, (left, right))

------------ TYPES ------------
type Tree = Map String (String, String)

type Input = ([Char], Tree)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getNeighbors :: Tree -> String -> (String, String)
getNeighbors m k = m Map.! k

getNext 'L' (l, _r) = l
getNext 'R' (_l, r) = r

navigate :: Tree -> [Char] -> (String, Int) -> (String, Int)
navigate m l ("ZZZ", steps) = ("ZZZ", steps)
navigate m (i : tail) (currNode, steps) =
  let next = getNext i $ getNeighbors m currNode
   in navigate m tail (next, steps + 1)

part1 :: Input -> OutputA
part1 (i, m) = snd $ navigate m (cycle i) ("AAA", 0)

------------ PART B ------------
navigate' m l (node@(_ : _ : ['Z']), steps) = (node, steps)
navigate' m (i : tail) (currNode, steps) =
  let next = getNext i $ getNeighbors m currNode
   in navigate' m tail (next, steps + 1)

part2 :: Input -> OutputB
part2 (i, m) =
  let startNodes = filter (\x -> (x !! 2) == 'A') $ Map.keys m
   in foldr (lcm . (\node -> snd $ navigate' m (cycle i) (node, 0))) 1 startNodes
