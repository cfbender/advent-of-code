module Days.Day02 where

--  Day 2! Okay, this was much better than yesterday.
--  Getting this parser written felt amazing, and I feel like I
--  actually learned how it's working a bit more. Still a long way to go
--  but I really needed this after yesterday

{- ORMOLU_DISABLE -}
import Data.Text (Text)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text 
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = game `sepBy` endOfLine
  where
    color = choice [string "red", string "blue", string "green"]
    colorCount = do
      x <- decimal
      space
      color <- color
      return (color, x)
    result =
      Map.fromList <$> colorCount `sepBy` string ", "
    cubeSet = do
      space
      result `sepBy` string "; "
    game = do
      string "Game"
      space
      id <- decimal
      string ":"
      sets <- cubeSet
      return (id, sets)

------------ TYPES ------------
type Input = [(Int, [Map Text Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
possible :: Map Text Int -> Bool
possible map =
  (Map.lookup "red" map <= Just 12)
    && (Map.lookup "green" map <= Just 13)
    && (Map.lookup "blue" map <= Just 14)

part1 :: Input -> OutputA
part1 = sum . map fst . filter (all possible . snd)

------------ PART B ------------
minimumColors :: [Map Text Int] -> [Int]
minimumColors sets =
  let red = maximum . mapMaybe (Map.lookup "red") $ sets
      green = maximum . mapMaybe (Map.lookup "green") $ sets
      blue = maximum . mapMaybe (Map.lookup "blue") $ sets
   in [red, green, blue]

part2 :: Input -> OutputB
part2 = sum . map (product . minimumColors . snd)
