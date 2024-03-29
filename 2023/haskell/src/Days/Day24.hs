module Days.Day24 where

-- well ain't this a treat way to wrap up the year
-- I liked the part 1, tried to do it with Cramer's rule
-- but I don't know shit about linear algebra so I just
-- did the y = mx + b route.
--
-- then, part 2.
--
-- never heard of z3, but it seemed like that was the only real way people
-- are doing it. so I did that here but this library BLOWS.
-- it's so out of date and nearly impossible to use, and it really
-- tested the limits of my haskell knowledge.
--
-- This works but it is horrifically slow, I ended up just reimplementing this in
-- python and got the answer instantly.
--
-- maybe there's a way to speed this up since they're both just using the same
-- C libraries underneath, but I don't care to learn anything more about this library.
--
-- in the future, if I need Z3 again I will be using python for sure
--
-- final day coming. finally.

import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, many1, sepBy, signed, space, string)
import Data.List (tails)
import Data.Maybe (catMaybes, mapMaybe)
import Program.RunDay qualified as R (Day, runDay)
import System.IO.Unsafe
import Util.Util qualified as U

-- import Z3.Monad

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = stone `sepBy` endOfLine
  where
    stone = do
      [x, y, z] <- map fromIntegral <$> decimal `sepBy` string ", "
      space
      char '@'
      many1 space
      [dx, dy, dz] <- map fromIntegral <$> signed decimal `sepBy` (char ',' >> many1 space)
      return $ Stone (x, y, z) (dx, dy, dz)

------------ TYPES ------------
data Hailstone = Stone (Double, Double, Double) (Double, Double, Double) deriving (Show, Eq)

type Input = [Hailstone]

type OutputA = Int

type OutputB = Integer

------------ PART A ------------
slope :: Hailstone -> Double
slope (Stone (x, y, _) (dx, dy, _)) = dy / dx

-- ax + b = cx + d
-- ax - cx = d - b
-- x(a - c) = d - b
-- x = (d - b) / (a - c)
crossing :: Hailstone -> Hailstone -> Maybe (Double, Double)
crossing a@(Stone (x, y, _) (dx, dy, _)) b@(Stone (x1, y1, _) (dx1, dy1, _))
  | s == s1 = Nothing
  | t1 < 0 || t2 < 0 = Nothing
  | otherwise = Just (ix, iy)
  where
    [s, s1] = map slope [a, b]
    [int, int1] = map (\(x, y, s) -> y - x * s) [(x, y, s), (x1, y1, s1)]
    ix = (int1 - int) / (s - s1)
    iy = s * ix + int
    t1 = (ix - x) / dx
    t2 = (ix - x1) / dx1

combinations list = [(a, b) | rest@(a : _) <- tails list, b <- rest]

part1 :: Input -> OutputA
part1 = length . filter inArea . mapMaybe (uncurry crossing) . combinations
  where
    inArea (x, y) = x >= 200000000000000 && x <= 400000000000000 && y >= 200000000000000 && y <= 400000000000000

------------ PART B ------------
-- NOTE: commenting out for compile on Mac
--
--
-- find coordinates of stone thrown that hits every hailstone
-- solver :: [Hailstone] -> Z3 (Maybe [Integer])
-- solver hs = do
--   x <- mkFreshBvVar "x" 64
--   y <- mkFreshIntVar "y"
--   z <- mkFreshIntVar "z"
--   dx <- mkFreshIntVar "dx"
--   dy <- mkFreshIntVar "dy"
--   dz <- mkFreshIntVar "dz"
--   -- for x y and z
--   assert
--     =<< mkAnd
--     =<< mapM
--       ( \(Stone (x'', y'', z'') (dx'', dy'', dz'')) -> do
--           -- add time for each stone t
--           t <- mkFreshIntVar "t"
--           x' <- mkRealNum x''
--           dx' <- mkRealNum dx''
--           y' <- mkRealNum y''
--           dy' <- mkRealNum dy''
--           z' <- mkRealNum z''
--           dz' <- mkRealNum dz''
--           px <- mkMul [t, dx]
--           gx <- mkAdd [x, px]
--           px' <- mkMul [t, dx']
--           py <- mkMul [t, dy]
--           gy <- mkAdd [y, py]
--           py' <- mkMul [t, dy']
--           pz <- mkMul [t, dz]
--           gz <- mkAdd [z, pz]
--           pz' <- mkMul [t, dz']
--           mkAnd
--             =<< sequence
--               -- assert time is in the future
--               [ mkGt t =<< mkIntNum 0,
--                 -- add constraint that each stone is hit by the formula
--                 -- x + tK  * dx == x' + tK * dx'
--                 mkEq gx =<< mkAdd [x', px'],
--                 mkEq gy =<< mkAdd [y', py'],
--                 mkEq gz =<< mkAdd [z', pz']
--               ]
--       )
--       hs
--   -- check and solve, return coordinates of thrown
--   fmap snd $ withModel $ \m ->
--     catMaybes <$> mapM (evalInt m) [x, y, z, dx, dy, dz]
--
-- run :: [Hailstone] -> IO [Integer]
-- run hs =
--   evalZ3 (solver hs) >>= \case
--     Nothing -> error "No solution found."
--     Just sol -> do
--       putStr "Solution: " >> print sol
--       return sol

part2 :: Input -> OutputB
part2 i = error "Part 2 solution commented out"
  where
    -- result =  sum $ take 3 $ unsafePerformIO (run checkStones)
    checkStones = take 3 i
