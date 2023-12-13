module Days.Day13 where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine, count,choice,  char, many1)
import Data.Void
import Data.Vector (Vector)
import qualified Data.Vector as V
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = pattern `sepBy` count 2 endOfLine
  where
    line = many1 (choice [char '#', char '.'])
    pattern = V.fromList <$> line `sepBy` endOfLine

------------ TYPES ------------
type Input = [Vector String]

type OutputA = String

type OutputB = Void

------------ PART A ------------
-- go through vecs
-- find 2 that match (m and n)
--
-- let (left, right) =
--    (\(a,b) -> (a, V.reverse $ V.drop 2 b)) $ V.partition (<m) rows
--
-- write fn to match list lengths
-- if right is longer, drop from left
-- if left is longer,  drop from right
--
-- check left == right
-- if that check passes, return value
-- else check next rows,
-- if still no value, transpose and check again
part1 :: Input -> OutputA
part1 = show

------------ PART B ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
