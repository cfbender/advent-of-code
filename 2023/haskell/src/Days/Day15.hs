module Days.Day15 where

-- fun little exercise here but a list of simple math operations
-- apparently breaks my human brain after 3 drinks
--
-- look, that's on me. I get it.
--
-- really fun tho! hoping for more around this level of effort lol


import Data.Attoparsec.Text
import Data.Char (ord, digitToInt)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Program.RunDay as R (runDay, Day)


runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` char ','
  where
    instruction = do
      label <- many1 letter
      operation <- choice [char '=', char '-']
      focalLength <- option Nothing (Just . digitToInt <$> digit)
      return (label, operation, focalLength)

------------ TYPES ------------
type Instruction = (String, Char, Maybe Int)

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
hash :: String -> Int
hash =
  foldl
    ( \acc c ->
        (`mod` 256)
          . (* 17)
          $ (+ acc)
          $ ord c
    )
    0

part1 :: Input -> OutputA
part1 = sum . map (hash . test)
  where
    test (l, o, f) = l ++ [o] ++ convert f
    convert (Just x) = show x
    convert Nothing = ""

------------ PART B ------------
step :: Map Int (Seq (String, Int)) -> Instruction -> Map Int (Seq (String, Int))
-- replace or add lens
step m (l, '=', Just f) = if hasLens then go replace else go add
  where
    go = flip (M.insert num) m
    num = hash l
    box = M.findWithDefault Seq.empty num m
    currLens = Seq.findIndexL ((== l) . fst) box
    hasLens = isJust currLens
    newLens = (l, f)
    replace = Seq.update (fromJust currLens) newLens box
    add = box Seq.|> newLens
-- remove lens
step m (l, '-', _) = M.alter remove (hash l) m
  where
    remove (Just s) = Just (Seq.filter ((/= l) . fst) s)
    remove Nothing = Nothing

power :: (Int, Seq (String, Int)) -> Int
power (box, ls) = sum $ focus ls
  where
    focus = map (* (box + 1)) . zipWith (*) [1 ..] . map snd . toList

part2 :: Input -> OutputB
part2 = sum . map power . M.toList . foldl step init
  where
    init = M.fromList [(0, Seq.empty)]
