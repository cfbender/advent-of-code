module Days.Day01 where

--  Day 1!
--  Writing this the next day since I forgot to collect my thoughts
--  but this day was a serious shock to the system. This language is
--  tough, and I am underprepared. I spent like 2 hours getting the parser
--  to run, and it's not even a very helpful parser. In the end I got it,
--  and hopefully the parsing isn't that hard later on but I'm sure it will be.

import Data.Attoparsec.Text
import Data.Char
import Data.List
import Data.Maybe
import Data.Text (Text, pack, replace, unpack)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (choice [letter, digit]) `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
ends list = [head list, last list]

part1 :: Input -> OutputA
part1 = sum . map ((read :: String -> Int) . ends . filter isDigit)

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

part2 :: Input -> OutputB
part2 = sum . map (undigits . ends . mapMaybe parseDigit . tails)
