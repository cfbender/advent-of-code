module Days.Day14 where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine)
import Data.Void
import Data.List (groupBy)
import Data.Map (Map, keys)
import qualified Data.Map as M
import Util.Parsers (coordinateParser)
import Data.Maybe (isJust)
import qualified Util.Util as U
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser rock 0
  where
    rock 'O' = Just Rock
    rock '#' = Just Cube
    rock _ = Nothing

------------ TYPES ------------
data Rock = Rock | Cube deriving (Show, Eq)

type Input = Map (Int, Int) Rock

type OutputA = Int

type OutputB = String

------------ PART A ------------

rollNorth :: Input -> Input
rollNorth m
  | all atRest (M.keys rocks) = m
  | otherwise = rollNorth rolled
  where
    atRest (_, 0) = True
    atRest (x, y) = isJust $ m M.!? (x, y - 1)
    rocks = M.filter (== Rock) m
    toRoll = M.filterWithKey (\k _ -> not $ atRest k) rocks
    rolled =
      M.union
        (M.mapKeys (\(x, y) -> (x, y - 1)) toRoll)
        (M.difference m toRoll)

measure :: Input -> Int
measure m =
  sum
    . M.elems
    $ M.mapWithKey (\y c -> (maxY - y + 1) * c) counts
  where
    counts = M.foldrWithKey count M.empty m
    count :: (Int, Int) -> Rock -> Map Int Int -> Map Int Int
    count (_, y) Rock m = M.alter inc y m
    count _ Cube m = m
    inc :: Maybe Int -> Maybe Int
    inc v = case v of
      Just x -> Just (x + 1)
      Nothing -> Just 1
    maxY = maximum . map (snd . fst) $ M.toList m

part1 :: Input -> OutputA
part1 = measure . rollNorth

------------ PART B ------------
part2 :: Input -> OutputB
part2 = error "Not implemented yet!"
