module Days.Day12 where

{- ORMOLU_DISABLE -}
import Control.Monad
import Data.List
import qualified Data.List  as List
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine, char, many1, choice, space, decimal)
import Data.Void
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Ord
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = springLine `sepBy` endOfLine
  where
    springLine = do
      springs <- many1 (choice [char '#', char '.', char '?'])
      space
      counts <- decimal `sepBy` char ','
      return (springs, counts)

------------ TYPES ------------

type Input = [(String, [Int])]

type OutputA = Int

type OutputB = String

------------ PART A ------------
-- springs is binary rep of filled springs list
-- groups is input (eg. 1,1,3)
check :: [Int] -> [Int] -> Bool
check groups springs = groups == sGroups
  where
    sGroups =
      map sum
        . filter (/= [0])
        $ groupBy (\a b -> a == 1 && b == 1) springs

-- decimal to binary list of 1 or 0
binary :: Int -> [Int]
binary 0 = [0]
binary n = go n
  where
    go 0 = []
    go k = mod k 2 : go (div k 2)

-- n is number of ?s
-- f is number missing
combos :: Int -> Int -> [[Int]]
combos n f = filter ((== f) . sum) $ replicateM n [0, 1]

replace :: String -> [Int] -> [Int]
replace [] [] = []
replace ('?' : r) (x : r') = x : replace r r'
replace ('.' : r) g = 0 : replace r g
replace ('#' : r) g = 1 : replace r g

ways springs groups =
  length
    . filter (check groups)
    . map (replace springs)
    $ combos unknowns missing
  where
    unknowns = length $ filter (== '?') springs
    present = length $ filter (== '#') springs
    missing = sum groups - present

partA :: Input -> OutputA
partA = sum . map (uncurry ways)

------------ PART B ------------
unfold :: (String, [Int]) -> (String, [Int])
unfold (s, g) = (intercalate "?" $ replicate 5 s, concat $ replicate 5 g)

-- this will take like 18 trillion years.
-- need to figure out DP/memoization on this
partB :: Input -> OutputB
partB = show . map (uncurry ways . unfold)
