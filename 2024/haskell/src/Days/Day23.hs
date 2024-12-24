module Days.Day23 where

-- so much like christmas day last year!
--
-- pretty cool - got to learn about Bron-Kerbosch which I had never seen before.
-- love the use of the word clique

import Data.Attoparsec.Text (Parser, endOfLine, letter, many1, sepBy)
import Data.List (intercalate, maximumBy, sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (around)
import Util.Util (uniq)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 letter `around` "-") `sepBy` endOfLine

------------ TYPES ------------
type Input = [(String, String)]

type OutputA = Int

type OutputB = String

------------ PART 1 ------------

bidirectional :: (String, String) -> [(String, Set String)]
bidirectional (n, m) = [(n, S.fromList [m]), (m, S.fromList [n])]

connected :: Map String (Set String) -> String -> [[String]]
connected i k = uniq $ concatMap find' checks
  where
    vs = i M.! k
    checks = S.toList vs
    find' :: String -> [[String]]
    find' x =
      let conn = S.toList (i M.! x)
       in map (\x' -> [k, x, x']) $ filter (`S.member` vs) conn

part1 :: Input -> OutputA
part1 i =
  length $
    filter (any tStart) $
      uniq $
        map sort $
          concatMap (filter ((>= 3) . length) . connected mapWires) (M.keys mapWires)
  where
    mapWires = M.fromListWith S.union $ concatMap bidirectional i
    tStart ('t' : _) = True
    tStart _ = False

------------ PART 2 ------------
bronKerbosch :: Map String (Set String) -> Set String -> Set String -> Set String -> [Set String]
bronKerbosch graph r p x
  | S.null p && S.null x = [r]
  | otherwise =
      let pivot = maximumBy (comparing (S.size . (graph M.!))) (S.union p x)
          pivotNeighbors = graph M.! pivot
          vertices = S.difference p pivotNeighbors
       in concatMap
            ( \v ->
                let neighbors = graph M.! v
                    newR = S.insert v r
                    newP = S.intersection p neighbors
                    newX = S.intersection x neighbors
                 in bronKerbosch graph newR newP newX
            )
            (S.toList vertices)

part2 :: Input -> OutputB
part2 i =
  intercalate "," $
    sort $
      S.toList $
        maximumBy (comparing S.size) $
          bronKerbosch mapWires S.empty (S.fromList $ M.keys mapWires) S.empty
  where
    mapWires = M.fromListWith S.union $ concatMap bidirectional i
    bidirectional (n, m) = [(n, S.fromList [m]), (m, S.fromList [n])]
