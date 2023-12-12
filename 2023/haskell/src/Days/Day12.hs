module Days.Day12 where

{- ORMOLU_DISABLE -}
import Data.List 
import qualified Data.List  as List
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine, char, many1, choice, space, decimal)
import Data.Void
import Data.Vector
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
      springs <- V.fromList . List.map spring <$> many1 (choice [char '#', char '.', char '?'])
      space
      counts <- decimal `sepBy` char ','
      return (springs, counts)
    spring '#' = Damaged
    spring '.' = Operational
    spring '?' = Unknown

------------ TYPES ------------
data Condition = Operational | Damaged | Unknown deriving (Show, Eq)

type Input = [(Vector Condition, [Int])]

type OutputA = String

type OutputB = Void

------------ PART A ------------
-- ideas:
-- guess and check, write function that checks end state
-- (ie that #.#.### satisfies 1,1,3), and then check all possibilities
--      so for ???.###
--        try: ....###
--             ..#.###
--             etc..
--             ###.###
--        somewhere in there would've been #.#.###
--        binary so it's just 2^3 things to check right?
--        full input looks like maybe ~15 max. that's 32k things to check woof
--        so maybe find an optimization on this to limit the search space
--        note: have to check full search space since enumerating combinations that work
--
--
-- something with a sliding window matching the group?
partA :: Input -> OutputA
partA = show

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
