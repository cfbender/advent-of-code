module Days.Day09 (runDay) where

-- a fun one! spent a lot of time fighting the compiler and types as usual
-- and tried to be cheeky with only taking the last two before I realized
-- that that only works up to the depth the pattern takes to resolve.
-- so I just did it the easy way and then thought to myself why didn't I do that
-- from the start!
--
-- really happy with how clean this solution came out though

{- ORMOLU_DISABLE -}
import Util.Util

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text 
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = filter (not . null) . map reverse <$> ((signed decimal `sepBy` char ' ') `sepBy` endOfLine)

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA

difference (x : [y]) = x - y

reduceLevel :: [Int] -> [Int]
reduceLevel = map difference . windows 2

findNext :: Int -> [Int] -> Int
findNext n l
  | all (== 0) l = n
  | otherwise = findNext (n + head l) (reduceLevel l)

partA = sum . map (findNext 0)

------------ PART B ------------

partB :: Input -> OutputB
partB = sum . map (findNext 0 . reverse)
