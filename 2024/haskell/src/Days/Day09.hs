module Days.Day09 where

-- good golly helping these elves is hard
--
-- I used so many bad data structures on this one until I got to whatever the hell this is
-- this is so slow but it works, and that's good enough for me rn.
-- I actually really like my part 1, and my part 2 ended up like what my part 1 started as which is cool,
-- but I wish it was faster.
--
-- I feel like this was really hard without mutability. I mean the whole problem is like about
-- mutating data. maybe I should've used something like nested lists rather than continuous blocks?
--
-- idk but this is working now.

import Data.Attoparsec.Text (Parser, digit, endOfLine, many1, sepBy)
import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ord qualified as Ord
import Data.Vector (Vector)
import Data.Vector qualified as V
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = V.fromList . zipWith (curry convert) [0 ..] <$> many1 digit
  where
    convert (i, x)
      | even i = CFile (i `div` 2, digitToInt x)
      | otherwise = CEmpty (digitToInt x)

------------ TYPES ------------
type Input = Vector CBlock

type OutputA = Int

type OutputB = Int

-- File ID or Empty
data Block = File Int | Empty deriving (Show, Eq)

-- C for contiguous blocks of space
data CBlock = CFile (Int, Int) | CEmpty Int deriving (Show, Eq, Ord)

instance Ord Block where
  compare Empty _ = GT
  compare _ Empty = LT
  compare _ _ = EQ

------------ PART 1 ------------
defrag :: [Block] -> [Block]
defrag xs = take count $ reverse $ fst $ foldl step ([], stack) xs
  where
    stack = reverse (filter (/= Empty) xs)
    count = length stack
    -- if block is empty, pop file from stack and replace
    step (acc, f : rest) Empty = (f : acc, rest)
    step (acc, stack) x = (x : acc, stack)

checksum :: [Block] -> Int
checksum = sum . zipWith (*) [0 ..] . map convert
  where
    convert (File x) = x
    convert Empty = 0

ungroup :: [CBlock] -> [Block]
ungroup = concatMap convert
  where
    convert (CFile (x, i)) = replicate i (File x)
    convert (CEmpty i) = replicate i Empty

part1 :: Input -> OutputA
part1 = checksum . defrag . ungroup . V.toList

------------ PART 2 ------------
replace :: Vector CBlock -> CBlock -> CBlock -> Vector CBlock
replace xs a b = V.map (\x -> if x == a then b else x) xs

move :: Vector CBlock -> CBlock -> CBlock -> Vector CBlock
move xs (CEmpty eSize) (CFile (fId, fSize)) = newLeft V.++ padding V.++ rest
  where
    -- split at where the hole is
    (left, right) = V.break (== CEmpty eSize) xs
    -- take the right side and drop the hole, and replace the file with empty space
    rest = replace (V.drop 1 right) (CFile (fId, fSize)) (CEmpty fSize)
    -- add extra padding if the hole is bigger than the file
    padding = V.fromList [CEmpty (eSize - fSize) | eSize > fSize]
    -- add the moved file to the end of the left
    newLeft = V.snoc left (CFile (fId, fSize))

defrag' :: Vector CBlock -> Vector CBlock
defrag' xs = foldl step xs stack
  where
    isFile (CEmpty _) = False
    isFile _ = True
    stack = sortBy (comparing Ord.Down) (filter isFile $ V.toList xs)
    canFit x (CEmpty y) = x <= y
    canFit _ _ = False
    step :: Vector CBlock -> CBlock -> Vector CBlock
    -- for each file on the stack, find the first empty block to move it to
    step acc (CFile (id, size)) =
      -- look only to the left of the file
      let (left, _) = V.break (== CFile (id, size)) acc
       in case V.find (canFit size) left of
            Just (CEmpty x) -> move acc (CEmpty x) (CFile (id, size))
            Nothing -> acc

part2 :: Input -> OutputB
part2 = checksum . ungroup . V.toList . defrag'
