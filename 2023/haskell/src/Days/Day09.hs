module Days.Day09 where

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
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = filter (not . null) . map reverse <$> ((signed decimal `sepBy` char ' ') `sepBy` endOfLine)

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
findNext :: Int -> [Int] -> Int
findNext n (0 : [0]) = n
findNext n l = findNext (n + head l) [x - y | [x, y] <- windows 2 l]

part1 :: Input -> OutputA
part1 = sum . map (findNext 0)

------------ PART B ------------

part2 :: Input -> OutputB
part2 = sum . map (findNext 0 . reverse)
