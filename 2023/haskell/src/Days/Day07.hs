module Days.Day07 (runDay) where

-- okay this one was super fun. again hoping for more difficulty like this.
-- got to implement my first data type and parse into it, so learned a lot!
-- gold plated the HELL out of this, but I really had fun doing it and learned
-- a lot, which is exactly what I do AoC for. so perfect!

{- ORMOLU_DISABLE -}
import Data.Bifunctor (first)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
mapCards :: Char -> Card
mapCards card = case card of
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace

inputParser :: Parser Input
inputParser = game `sepBy` endOfLine
  where
    game = do
      hand <- many1 (choice [letter, digit])
      space
      bid <- decimal
      return (map mapCards hand, bid)

------------ TYPES ------------
type Input = [([Card], Int)]

type OutputA = Int

type OutputB = Int

data Hand
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Show, Eq, Ord)

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)

------------ PART A ------------
-- get frequencies of cards sorted to return hand result
scoreHand :: [Card] -> Hand
scoreHand cards =
  let results = sort . Map.elems . U.freq $ cards
   in case results of
        [1, 1, 1, 1, 1] -> HighCard
        [1, 1, 1, 2] -> OnePair
        [1, 2, 2] -> TwoPair
        [1, 1, 3] -> ThreeOfKind
        [2, 3] -> FullHouse
        [1, 4] -> FourOfKind
        [5] -> FiveOfKind

compareHands (handA, _, resultA) (handB, _, resultB)
  | resultA > resultB = GT
  | resultB > resultA = LT
  | otherwise = tiebreak handA handB

-- check down the list to compare cards
tiebreak :: [Card] -> [Card] -> Ordering
tiebreak (a : restA) (b : restB)
  | a > b = GT
  | a < b = LT
  | otherwise = tiebreak restA restB
tiebreak [] [] = EQ

partA :: Input -> OutputA
partA input =
  sum
    . zipWith (*) scores
    . map U.tsnd
    . sortBy compareHands
    . map (\(hand, bid) -> (hand, bid, scoreHand hand))
    $ input
  where
    scores = [1 .. length input]

------------ PART B ------------
replaceJack Jack = Joker
replaceJack x = x

replaceJoker a Joker = a
replaceJoker _ a = a

-- don't check if no jokers
tryJokers hand = if Joker `elem` hand then tryJokers' hand else scoreHand hand

-- find max of Joker being replaced by every card Two .. Ace
tryJokers' :: [Card] -> Hand
tryJokers' hand =
  snd
    . maximumBy (\(hA, rA) (hB, rB) -> compareHands (hA, 0, rA) (hB, 0, rB))
    $ replaced
  where
    scoreWithHand origHand newHand = (origHand, scoreHand newHand)
    replaced =
      map
        ( scoreWithHand hand
            . (\c -> map (replaceJoker c) hand)
        )
        [Two .. Ace]

partB :: Input -> OutputB
partB input =
  sum
    . zipWith (*) scores
    . map U.tsnd
    . sortBy compareHands
    . map
      ( \(hand, bid) ->
          ( map replaceJack hand,
            bid,
            tryJokers . map replaceJack $ hand
          )
      )
    $ input
  where
    scores = [1 .. length input]
