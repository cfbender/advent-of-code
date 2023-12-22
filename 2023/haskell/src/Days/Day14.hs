module Days.Day14 where

-- cycle detection is a first for me, though I didn't write this algorithm myself
-- this was a cool one, stayed up waayyyy too late for it tho.
-- for some reason I was thinking they all just settled into one state rather than cycling
-- but once I got that and knew to carry through the cycle count to then mod 1B off it
-- it wasn't too bad to figure out
--
-- just the old usual fighting the compiler like crazy and not being able to think in terms
-- of one big function composition just yet. getting there.

import Data.Attoparsec.Text (Parser, endOfLine, sepBy)
import Data.Map (Map, keys)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (coordinateParser)

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

data Direction = North | West | South | East deriving (Enum, Show, Eq)

type Input = Map (Int, Int) Rock

type OutputA = Int

type OutputB = Int

------------ PART A ------------

move :: Direction -> (Int, Int) -> (Int, Int)
move North (x, y) = (x, y - 1)
move West (x, y) = (x - 1, y)
move South (x, y) = (x, y + 1)
move East (x, y) = (x + 1, y)

atRest :: (Int, Int) -> Direction -> Map (Int, Int) Rock -> (Int, Int) -> Bool
atRest _ North _ (_, 0) = True
atRest _ North m c = isJust $ m M.!? move North c
atRest _ West _ (0, _) = True
atRest _ West m c = isJust $ m M.!? move West c
atRest (_, maxY) South m c@(_, y) = (y == maxY) || isJust (m M.!? move South c)
atRest (maxX, _) East m c@(x, _) = (x == maxX) || isJust (m M.!? move East c)

roll :: (Int, Int) -> Direction -> Input -> Input
roll b d m
  | all (atRest b d m) (M.keys rocks) = m
  | otherwise = roll b d rolled
  where
    rocks = M.filter (== Rock) m
    toRoll = M.filterWithKey (\k v -> not $ atRest b d m k) rocks
    rolled =
      M.union
        (M.mapKeys (move d) toRoll)
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
part1 i = measure $ roll bounds North i
  where
    maxX = maximum . map (fst . fst) $ M.toList i
    maxY = maximum . map (snd . fst) $ M.toList i
    bounds = (maxX, maxY)

------------ PART B ------------
findCycle :: (Eq a) => (Eq b) => [(a, b)] -> ([(a, b)], [(a, b)])
findCycle xxs = fCycle xxs xxs
  where
    fCycle ((_, x) : xs) (_ : (_, y) : ys)
      | x == y = fStart xxs xs
      | otherwise = fCycle xs ys
    fCycle _ _ = (xxs, []) -- not cyclic
    fStart (x'@(_, x) : xs) ((_, y) : ys)
      | x == y = ([], x' : fLength x xs)
      | otherwise = let (as, bs) = fStart xs ys in (x' : as, bs)
    fLength x (y'@(_, y) : ys)
      | x == y = []
      | otherwise = y' : fLength x ys

part2 :: Input -> OutputB
part2 i =
  (measure . snd . findBillionth . snd)
    . findCycle
    . scanl (\(count, m) c -> (count + 1, foldl (flip (roll bounds)) m c)) (0, i)
    $ repeat [North .. East]
  where
    measure' (c, m) = (c, measure m)
    findBillionth l =
      let start = (fst $ head l)
       in l !! mod (1_000_000_000 - start) (length l)
    maxX = maximum . map (fst . fst) $ M.toList i
    maxY = maximum . map (snd . fst) $ M.toList i
    bounds = (maxX, maxY)
