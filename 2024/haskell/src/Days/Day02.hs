module Days.Day02 (runDay) where

-- Not a bad day 2, pretty fun one! Still just a lot of time fighting the compiler
-- as I ride the bumpy start of learning Haskell again. Loving it tho, it writes such pretty code
-- even when my example here is pretty ugly.
-- Onward!

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Ix
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = filter ((> 1) . length) <$> report `sepBy` endOfLine
 where
  report = decimal `sepBy` char ' '

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

data Finding = Safe | Unsafe
  deriving (Show, Eq)

------------ PART A ------------
valid :: Int -> Int -> Bool
valid x y = inRange (1, 3) (abs (x - y))

check :: Finding -> [Int] -> Finding
check Unsafe _ = Unsafe
check Safe (x : y : z : xs) = check safety (y : z : xs)
 where
  safe = valid x y && valid y z
  safety = if safe && ((z > y && y > x) || (z < y && y < x)) then Safe else Unsafe
-- getting to a 2 length list means we've compared all the windows
check Safe [_, _] = Safe

partA :: Input -> OutputA
partA = length . filter (== Safe) . map (check Safe)

------------ PART B ------------
partB :: Input -> OutputB
partB input = length $ mapMaybe (find (== Safe)) results
 where
  results :: [[Finding]]
  results = map (map (check Safe)) tests
  tests :: [[[Int]]]
  -- get all the permutations of removing an element
  tests = map permute input
  permute :: [Int] -> [[Int]]
  -- generate combinations including the original list to check first
  permute xs = xs : [deleteAt n xs | n <- [0 .. length xs - 1]]
  deleteAt n xs =
    let (ys, zs) = splitAt n xs
     in ys ++ tail zs
