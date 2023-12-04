module Days.Day04 (runDay) where

-- I liked this day! Though I feel like the explanations could've been a bit clearer.
-- Maybe it's just hard to walk that line, because if he had said it was
-- 1 * 2 * 2 * 2 for 4 winning numbers then it's too obvious it's 2^3? idk
--
-- Also got a little tripped up on the parsing (wasn't flexible enough with number of spaces)
-- and the second part, but this was a fun one! Honestly wish every day this month
-- was like the last two - but I know the storm is coming.

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

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
partA :: Input -> OutputA
partA =
  sum
    . map ((2 ^) . subtract 1 . length)
    . filter (not . null)
    . map (\(_, win, own) -> own `intersect` win)

------------ PART B ------------
partB :: Input -> OutputB
addWinning :: Map Int Int -> Int -> [Int]
addWinning m id = newCards ++ concatMap (addWinning m) newCards
  where
    -- get the count of matching numbers from the passed card
    count = Map.findWithDefault 0 id m
    newCards = [(id + 1) .. (id + count)]
partB i = length cards + length (concatMap (addWinning cardMap . fst) cards)
  where
    cardMap = Map.fromList cards
    cards = map (\(id, win, own) -> (id, length $ own `intersect` win)) i
