module Days.Day04 where

-- I liked this day! Though I feel like the explanations could've been a bit clearer.
-- Maybe it's just hard to walk that line, because if he had said it was
-- 1 * 2 * 2 * 2 for 4 winning numbers then it's too obvious it's 2^3? idk
--
-- Also got a little tripped up on the parsing (wasn't flexible enough with number of spaces)
-- and the second part, but this was a fun one! Honestly wish every day this month
-- was like the last two - but I know the storm is coming.


import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text


runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = card `sepBy` endOfLine
  where
    numbers = decimal `sepBy` many1 space
    card = do
      string "Card"
      many1 space
      id <- decimal
      string ":"
      many1 space
      winning <- numbers
      space
      string "|"
      many1 space
      myNumbers <- numbers
      return (id, winning, myNumbers)

------------ TYPES ------------
type Input = [(Int, [Int], [Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
part1 :: Input -> OutputA

score [] = 0
score l = (2 ^) . subtract 1 $ length l

part1 =
  sum
    . map (\(_, win, own) -> score (own `intersect` win))

------------ PART B ------------
part2 :: Input -> OutputB
addWinning :: Map Int Int -> Int -> [Int]
addWinning m id = newCards ++ concatMap (addWinning m) newCards
  where
    -- get the count of matching numbers from the passed card
    count = Map.findWithDefault 0 id m
    newCards = [(id + 1) .. (id + count)]
part2 i = length cards + length (concatMap (addWinning cardMap . fst) cards)
  where
    cardMap = Map.fromList cards
    cards = map (\(id, win, own) -> (id, length $ own `intersect` win)) i
