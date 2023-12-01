module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Text (replace, unpack, pack,  Text)
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text 
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (choice [letter, digit]) `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
ends list = [head list, last list]

partA :: Input -> OutputA
partA = sum . map ((read :: String -> Int) . ends . filter isDigit)

------------ PART B ------------
convert :: String -> String
convert input = foldl (\acc (s, r) -> unpack . replace s r . pack $ acc) input digits
  where
    digits :: [(Text, Text)]
    digits =
      [ ("one", "1"),
        ("two", "2"),
        ("three", "3"),
        ("four", "4"),
        ("five", "5"),
        ("six", "6"),
        ("seven", "7"),
        ("eight", "8"),
        ("nine", "9"),
        ("zero", "0")
      ]

partB :: Input -> OutputB
partB = sum . map ((read :: String -> Int) . ends . filter isDigit . convert)
