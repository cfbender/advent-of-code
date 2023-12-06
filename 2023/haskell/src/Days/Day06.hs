module Days.Day06 (runDay) where

-- AWWWWW YEAH BABY AND JUST LIKE THAT THEY REEL ME BACK IN
-- this is what I needed, just a nice simple one
-- that lets me write some very terse and pretty code
--
-- hell yea.

{- ORMOLU_DISABLE -}
import Data.Bifunctor (bimap)
import Data.List

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Time:"
  many1 space
  times <- decimal `sepBy` many1 space
  endOfLine
  string "Distance:"
  many1 space
  distances <- decimal `sepBy` many1 space
  return (zip times distances)

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
traveled total spent = (total - spent) * spent

partA :: Input -> OutputA
partA = product . map (\(x, dist) -> length . filter (> dist) $ map (traveled x) [1 .. x - 1])

------------ PART B ------------
undigits :: [Int] -> Int
undigits = read . concatMap show

partB :: Input -> OutputB
partB i =
  let (time, dist) = bimap undigits undigits $ unzip i
      Just inflection = find (\x -> traveled time x > dist) [1 .. time - 1]
   in time - (inflection * 2) + 1
