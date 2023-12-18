module Days.Day13 where

-- this was fun! really slow time because life and work got in the way
-- but I enjoyed it. another where I stepped away to think about it
-- for a while, and that was nice.
--
-- think I got a pretty cute solution but it could probably be way cleaner
-- ah well I'll get better at this language later


import Data.List (transpose, find)
import Data.Maybe (fromJust, isJust)
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, sepBy, endOfLine, count,choice,  char, many1)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Array as Array


runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = pattern `sepBy` count 2 endOfLine
  where
    line = many1 (choice [char '#', char '.'])
    pattern = V.fromList <$> line `sepBy` endOfLine

------------ TYPES ------------
data Reflection = Horizontal | Vertical deriving (Show, Eq)

type Input = [Vector String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
matchLength :: (Show a) => Vector a -> Vector a -> (Vector a, Vector a)
matchLength a b
  -- drop diff from longer
  | lengthA > lengthB = (V.drop (lengthA - lengthB) a, b)
  | lengthB > lengthA = (a, V.drop (lengthB - lengthA) b)
  | otherwise = (a, b)
  where
    lengthA = length a
    lengthB = length b

checkMatch :: Vector (Int, String) -> Int -> Int -> Bool
checkMatch v a b = left == right
  where
    (l, r) =
      ( \(l, r) ->
          -- match the length to trim ones where the pattern would start over
          matchLength l $
            -- drop two from the right half to drop the two that already match
            -- and reverse it
            V.reverse (V.drop 2 r)
      )
        -- partition the vector into two vectors that
        -- are up to the left of the line and after it
        $ V.partition ((< a) . fst) v
    left = V.map snd l
    right = V.map snd r

findMatch :: Reflection -> (Reflection, Int) -> Vector String -> Maybe (Reflection, Int)
findMatch t check v = go (V.toList indexed)
  where
    indexed = V.indexed v
    lv = length v
    go ((xa, a) : next@(xb, b) : rest)
      -- two rows/cols match, check the surroundings for a full match
      | a == b && checkMatch indexed xa xb && (t, xb) /= check = Just (t, xb)
      | otherwise = go (next : rest)
    -- if still no value, transpose and check again
    go [_] = if t == Vertical then Nothing else findMatch Vertical check (V.fromList . transpose $ V.toList v)

summarize (Vertical, x) = x
summarize (Horizontal, x) = x * 100

part1 :: Input -> OutputA
part1 = sum . map (summarize . fromJust . findMatch Horizontal (Horizontal, 0))

------------ PART B ------------
smudge :: Vector String -> (Reflection, Int) -> (Reflection, Int)
smudge v original = fromJust $ fromJust $ find isJust trials
  where
    lv = length v
    lx = length (v V.! 0)
    bounds = ((0, 0), (lv - 1, lx - 1))
    trials =
      [ findMatch Horizontal original $
          replace (y, x)
        | (y, x) <- Array.range bounds
      ]
    replace (y, x) = v V.// [(y, rowCleaned)]
      where
        row = V.fromList (v V.! y)
        value = row V.! x
        cleaned = clean value
        rowCleaned = V.toList $ row V.// [(x, cleaned)]
    clean '#' = '.'
    clean '.' = '#'

part2 :: Input -> OutputB
part2 =
  sum . map (\a -> summarize . smudge a $ first a)
  where
    first = fromJust . findMatch Horizontal (Horizontal, 0)
