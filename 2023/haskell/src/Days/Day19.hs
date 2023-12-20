module Days.Day19 where

-- yeah I'm getting dumber for sure.
-- this one was so impossible for me to think through, and nothing really helped.
-- I effectively just yoinked and re-wrote https://github.com/alexjercan/aoc-2023/blob/master/src/Day19.hs
-- so thanks to alex for the help there, hope you don't mind me using your code.
--
-- almost quit on today, but I will soldier on.

import Control.Applicative.Combinators (between)
import Data.Attoparsec.Text
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Program.RunDay qualified as R (Day, runDay)
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  workflows <- workflowParser `sepBy` endOfLine
  count 2 endOfLine
  ratings <- ratingParser `sepBy` endOfLine
  pure (Map.fromList workflows, ratings)
  where
    workflowParser = do
      name <- many1 letter
      rules <- splitOn "," <$> between (char '{') (char '}') (many1 (notChar '}'))
      return (fall name, Workflow (fall name) (map rule (init rules)) (fall (last rules)))
    rule arg@(t : c : rest) =
      let [d, n] = splitOn ":" rest
          c' '>' = GT
          c' '<' = LT
          t' 'x' = XCool
          t' 'm' = Musical
          t' 'a' = Aerodynamic
          t' 's' = Shiny
       in Step (Condition (t' t) (c' c) (read d)) (fall n)
    fall "A" = Accept
    fall "R" = Reject
    fall n = Next n
    ratingParser = do
      string "{x="
      x <- decimal
      string ",m="
      m <- decimal
      string ",a="
      a <- decimal
      string ",s="
      s <- decimal
      char '}'
      return (x, m, a, s)

------------ TYPES ------------
data Param = XCool | Musical | Aerodynamic | Shiny deriving (Show, Eq, Enum)

data Next = Accept | Reject | Next String deriving (Show, Eq, Ord)

data Condition = Condition Param Ordering Int deriving (Show, Eq)

data Step = Step Condition Next deriving (Show, Eq)

data Workflow = Workflow Next [Step] Next deriving (Show, Eq)

type Rating = (Int, Int, Int, Int)

data RatingRange = RatingRange (Int, Int) (Int, Int) (Int, Int) (Int, Int) deriving (Show, Eq)

type Input = (Map Next Workflow, [Rating])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
ruleCompare :: Int -> Int -> Ordering -> Bool
ruleCompare a b LT = a < b
ruleCompare a b GT = a > b

check :: Map Next Workflow -> Workflow -> Rating -> Bool
check input (Workflow _ rules fallback) (x, m, a, s) =
  maybe
    (fall fallback)
    next
    passes
  where
    fall Accept = True
    fall Reject = False
    fall f = check input (input Map.! f) (x, m, a, s)
    next (Step _ Accept) = True
    next (Step _ Reject) = False
    next (Step _ n) = check input (input Map.! n) (x, m, a, s)
    passes =
      find
        ( \(Step (Condition param order value) _) ->
            case param of
              XCool -> ruleCompare x value order
              Musical -> ruleCompare m value order
              Aerodynamic -> ruleCompare a value order
              Shiny -> ruleCompare s value order
        )
        rules

addPart :: Rating -> Int
addPart (x, m, a, s) = x + m + a + s

part1 :: Input -> OutputA
part1 (input, parts) = sum $ map addPart $ filter (check input start) parts
  where
    start = input Map.! Next "in"

------------ PART B ------------
get XCool (RatingRange x _ _ _) = x
get Musical (RatingRange _ m _ _) = m
get Aerodynamic (RatingRange _ _ a _) = a
get Shiny (RatingRange _ _ _ s) = s

switch :: Param -> RatingRange -> (Int, Int) -> RatingRange
switch p (RatingRange x m a s) v = case p of
  XCool -> RatingRange v m a s
  Musical -> RatingRange x v a s
  Aerodynamic -> RatingRange x m v s
  Shiny -> RatingRange x m a v

pass r = (Nothing, Just r)

shrink r = (Just r, Nothing)

conditionRange :: Condition -> RatingRange -> (Maybe RatingRange, Maybe RatingRange)
conditionRange (Condition p LT x) r
  -- condition is checking outside of possible values
  | x <= x1 = pass r
  -- value must be less than x, and our range includes some values
  | x > x2 = shrink r
  -- x is somewhere between x1 and x2, split it into the values from
  -- the lower bound of our range to the bottom of x, and the rest of
  -- our bound (x to the top of our range)
  | otherwise = split
  where
    (x1, x2) = get p r
    split = (Just (switch p r (x1, x - 1)), Just (switch p r (x, x2)))
conditionRange (Condition p GT x) r
  -- condition is checking outside of possible values
  | x >= x2 = pass r
  -- value must be greater than x, and our range includes some values
  | x < x1 = shrink r
  -- x is somewhere between x1 and x2, split it into the values from
  -- the condition to the top of our current range, and the rest from
  -- our lower bound (the bottom of our range to the condition)
  | otherwise = split
  where
    (x1, x2) = get p r
    split = (Just (switch p r (x + 1, x2)), Just (switch p r (x1, x)))

rulesRange :: [Step] -> RatingRange -> Next -> [(RatingRange, Next)]
rulesRange [] r def = [(r, def)]
-- split the ranges based on the condition for that step
rulesRange (Step c n : rs) r def = case conditionRange c r of
  -- range already encompasses rule, no need to split, so continue
  (Nothing, Just r') -> rulesRange rs r' def
  -- range does not encompass rule, so return
  (Just r', Nothing) -> [(r', n)]
  -- range is split, so concat and recurse
  (Just r1, Just r2) -> (r1, n) : rulesRange rs r2 def

workflowRange' :: Map Next Workflow -> RatingRange -> Next -> [(RatingRange, Next)]
workflowRange' _ r Accept = [(r, Accept)]
workflowRange' _ r Reject = [(r, Reject)]
-- if not terminating, fallback to split ranges for the given step
workflowRange' ws p n = case Map.lookup n ws of
  Just (Workflow _ rs fall) -> rulesRange rs p fall

workflowRange :: Map Next Workflow -> [(RatingRange, Next)] -> [(RatingRange, Next)]
workflowRange ws = concatMap (uncurry (workflowRange' ws))

simulate :: Map Next Workflow -> RatingRange -> Int
simulate ws p =
  -- starting with "in", go until a workflow returns Accept or Reject
  let rns = until (all (\(_, n) -> n == Accept || n == Reject)) (workflowRange ws) [(p, Next "in")]
      -- find all ones that end in Accept
      rns' = map fst $ filter (\(_, n) -> n == Accept) rns
   in -- summarize the ranges by mapping them all into the product of their bounds
      sum $ map (\(RatingRange (x1, x2) (m1, m2) (a1, a2) (s1, s2)) -> (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)) rns'

part2 :: Input -> OutputB
part2 (input, _) = simulate input (RatingRange (1, 4000) (1, 4000) (1, 4000) (1, 4000))
