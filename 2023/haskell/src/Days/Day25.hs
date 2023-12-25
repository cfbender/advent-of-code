module Days.Day25 where

import Control.Parallel.Strategies (parBuffer, rdeepseq, withStrategy)
import Data.Attoparsec.Text (Parser, char, endOfLine, letter, many1, sepBy, string)
import Data.Foldable (find)
import Data.Graph (Edge, Graph, Tree (Node))
import Data.Graph qualified as G
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Arr (Array (Array))
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = connected `sepBy` endOfLine
  where
    connected = do
      name <- many1 letter
      string ": "
      conn <- many1 letter `sepBy` char ' '
      return (name, conn)

------------ TYPES ------------
type Input = [(String, [String])]

type OutputA = String

type OutputB = String

------------ PART A ------------
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = map (x :) (combinations (k - 1) xs) ++ combinations k xs

uniqueCombinations k xs = filter (\c -> length c == k) $ nub $ combinations k xs

componentCount :: Graph -> Int
componentCount = length . G.scc

removeEdges :: [Edge] -> [Edge] -> [Edge]
removeEdges es toRemove = filter (`notElem` toRemove) es

verticesInComponents :: Graph -> [Int]
verticesInComponents = map length . G.scc

-- found using GraphViz
-- remove gbc -> hxr
-- remove xkz -> mvv
-- remove tmt -> pnz
part1 :: Input -> OutputA
part1 i = show $ product $ verticesInComponents filtered
  where
    (graph, _, _) = G.graphFromEdges $ map (\(k, v) -> (k, k, v)) $ M.toList mapWires
    (filtered, _, _) =
      G.graphFromEdges $
        map (\(k, v) -> (k, k, v)) $
          M.toList $
            M.mapWithKey
              ( \k v ->
                  case k of
                    "gbc" -> filter (/= "hxr") v
                    "hxr" -> filter (/= "gbc") v
                    "xkz" -> filter (/= "mvv") v
                    "mvv" -> filter (/= "xkz") v
                    "tmt" -> filter (/= "pnz") v
                    "pnz" -> filter (/= "tmt") v
                    s -> v
              )
              mapWires
    mapWires = M.fromListWith (++) $ concatMap bidirectional i
    bidirectional node@(n, cs) = node : [(c, [n]) | c <- cs]
    -- brute force implementation left to show at least I tried 🙃
    buildGraph = G.buildG (lb, ub)
    wires =
      find ((== 2) . componentCount . G.buildG (lb, ub) . removeEdges edges) $
        withStrategy (parBuffer 10_000 rdeepseq) tests
    final = buildGraph . removeEdges edges <$> wires
    edges = G.edges graph
    Array lb ub _ _ = graph
    tests = uniqueCombinations 3 $ G.edges graph

------------ PART B ------------
part2 :: Input -> OutputB
part2 _ = "We did it!"
