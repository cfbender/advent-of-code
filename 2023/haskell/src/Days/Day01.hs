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
undigits :: [Int] -> Int
undigits digits = read $ concatMap show digits

parseDigit :: String -> Maybe Int
parseDigit [] = Nothing
parseDigit input@(char : _)
  | isDigit char = Just $ digitToInt char
  | "one" `isPrefixOf` input = Just 1
  | "two" `isPrefixOf` input = Just 2
  | "three" `isPrefixOf` input = Just 3
  | "four" `isPrefixOf` input = Just 4
  | "five" `isPrefixOf` input = Just 5
  | "six" `isPrefixOf` input = Just 6
  | "seven" `isPrefixOf` input = Just 7
  | "eight" `isPrefixOf` input = Just 8
  | "nine" `isPrefixOf` input = Just 9
  | otherwise = Nothing

partB :: Input -> OutputB
partB = sum . map (undigits . ends . mapMaybe parseDigit . tails)
