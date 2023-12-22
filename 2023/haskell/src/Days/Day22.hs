module Days.Day22 where

-- this one wasn't bad, a bit much to think through
-- which made me make a slower solution in order to
-- be able to think through it properly.
-- be sure to run this with +RTS -N10 (or whatever number of cores you have)
-- to get it done in a reasonable amount of time lol
--
-- still really may just do an all silver-star run next year

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Parsers (Coordinate, around)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = M.fromList . zipWith (curry toCubes) ['A' ..] <$> brick `sepBy` endOfLine
  where
    brick = coordinate `around` char '~'
    coordinate = do
      x <- decimal
      char ','
      y <- decimal
      char ','
      z <- decimal
      return (x, y, z)
    toCubes (a, ((x, y, z), (x', y', z'))) =
      ( a,
        S.fromList $
          [Cube x y z | x <- [x .. x'], y <- [y .. y'], z <- [z .. z']]
      )

------------ TYPES ------------t
data Cube = Cube Int Int Int deriving (Show, Ord, Eq)

type Brick = Set Cube

type Input = Map Char Brick

type OutputA = Int

type OutputB = Int

------------ PART A ------------
converge :: (Eq a) => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

fall :: Input -> Char -> Brick -> Brick
fall i n b = if collided then b else next
  where
    next = S.map (\c@(Cube x y z) -> if z == 1 then c else Cube x y (z - 1)) b
    -- remove element so it can't collide with itself if it's vertical
    collided = any (\s -> not (s `S.disjoint` next)) $ M.elems $ M.delete n i

settle :: Input -> Input
settle bs = M.mapWithKey (fall bs) bs

above :: Input -> Char -> Brick -> Input
-- bricks that intersect with the virtual brick are supported by the original
above i n b = M.filter (\b' -> not $ b' `S.disjoint` check) others
  where
    others = M.delete n i
    -- make virtual brick one layer up
    check = S.map (\(Cube x y z) -> Cube x y (z + 1)) b

supporting :: Input -> Map Char (Set Char)
supporting i = supportedBy
  where
    supports = M.mapWithKey (\k v -> map fst $ M.toList $ above i k v) i
    -- map of bricks to the set of bricks that support them
    supportedBy = M.map S.fromList $ M.fromListWith (++) $ concatMap (\(k, vs) -> map (,[k]) vs) $ M.toList supports

part1 :: Input -> OutputA
part1 i =
  length
    -- filter the bricks to show ones
    -- that are the only one supporting another brick
    $ filter
      ( \n ->
          M.empty
            == M.filter (\v -> v == S.singleton n) supportedBy
      )
    $ M.keys settled
  where
    settled = converge settle i
    supportedBy = supporting settled

------------ PART B ------------
different :: Input -> Input -> Int
different a b = length $ M.filterWithKey (\k v -> v /= M.findWithDefault S.empty k a) b

chainReaction :: Input -> Char -> Int
chainReaction i n = different deleted $ converge settle deleted
  where
    deleted = M.delete n i

part2 :: Input -> OutputB
part2 i = sum $ parMap rdeepseq (chainReaction settled) bricks
  where
    aDeleted = M.delete 'C' settled
    bricks = M.keys settled
    settled = converge settle i
    supportedBy = supporting settled
