module Days.Day08 (runDay) where

-- how do you, as a grown adult, re-learn math?
-- I mean seriously. how do I learn to think math-first?
-- it would've taken me a while to think of the LCM and idk why.
-- but thank goodness for hints.
--
-- anyway this was fun, more gold plating with the Graph, and I learned a lot
-- in the intermediate solutions so that's cool

{- ORMOLU_DISABLE -}
import Data.Graph
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser Input
inputParser = do
  instructions <- many1 (choice [char 'R', char 'L'])
  count 2 endOfLine
  mapLines <- mapLine `sepBy` endOfLine
  return (instructions, graphFromEdges $ edgeList mapLines)
  where
    edgeList mapLines = [(node, node, [left, right]) | (node, (left, right)) <- mapLines]
    mapLine = do
      node <- count 3 letter
      string " = ("
      left <- count 3 letter
      string ", "
      right <- count 3 letter
      char ')'
      return (node, (left, right))

------------ TYPES ------------
type VertexMap = String -> Maybe Vertex

instance Show VertexMap where
  show v = "VertexMap"

type NodeMap = Vertex -> (String, String, [String])

instance Show NodeMap where
  show v = "NodeMap"

type Input = ([Char], (Graph, NodeMap, VertexMap))

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getNeighbors :: VertexMap -> NodeMap -> String -> (String, String)
getNeighbors v n k = case n <$> v k of
  Just (_, _, [left, right]) -> (left, right)

getNext 'L' (l, _r) = l
getNext 'R' (_l, r) = r

navigate :: VertexMap -> NodeMap -> [Char] -> (String, Int) -> (String, Int)
navigate v n l ("ZZZ", steps) = ("ZZZ", steps)
navigate v n (i : tail) (currNode, steps) =
  let next = getNext i $ getNeighbors v n currNode
   in navigate v n tail (next, steps + 1)

partA :: Input -> OutputA
partA (i, (g, n, v)) = snd $ navigate v n (cycle i) ("AAA", 0)

------------ PART B ------------
navigate' v n l (node@(_ : _ : ['Z']), steps) = (node, steps)
navigate' v n (i : tail) (currNode, steps) =
  let next = getNext i $ getNeighbors v n currNode
   in navigate' v n tail (next, steps + 1)

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

partB :: Input -> OutputB
partB (i, (g, n, v)) =
  let startNodes = filter (\x -> (x !! 2) == 'A') . map (U.tsnd . n) $ vertices g
   in foldr
        ( lcm
            . (\node -> snd $ navigate' v n (cycle i) (node, 0))
        )
        1
        startNodes
