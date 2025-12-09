module Days.Day06 where

-- ahhh a parsing nightmare, my favorite in haskell.
-- this really highlights my weakness here, and I struggled a lot to get it right.
-- I saw pretty early that the operators could help delimit the columns, but for some reason
-- I was stuck on getting it a different way. when I finally did it like that I got it pretty quickly.
-- I would say lesson learned but probably not

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, choice, count, decimal, digit, endOfLine, many', many1, sepBy, space)
import Data.List (null, transpose)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Util.Util (chunksOf, traceShowIdWithContext)

runDay :: R.Day
runDay = R.runDay inputParser part1 part2

------------ PARSER ------------
split :: String -> [Int] -> [String]
split s ops = reverse $ split' s [] ops
  where
    split' s a [] = a
    split' s a (o : os) =
        let x = take o s
            s' = drop (o + 1) s
         in split' s' (x : a) os

convert ' ' = Nothing
convert c = Just c

joinAll [ws, xs, ys, zs] = join ws xs ys zs
join (w : ws) (x : xs) (y : ys) (z : zs) (o : os) = (w, x, y, z, o) : join ws xs ys zs os
join [] [] [] [] [] = []

inputParser :: Parser Input
inputParser = do
    rows <- count 4 rowParser
    operators <- many1 operatorParser
    return (joinAll (map (convertNums operators) rows) (map fst operators))
  where
    convertNums :: [(Operator, Int)] -> String -> [[Maybe Char]]
    convertNums ops nums =
        let fix (x : xs) = (x + 1) : xs
            fixedOps = reverse $ fix $ reverse $ map snd ops
         in map (map convert) $ split nums fixedOps
    rowParser = do
        nums <- many1 (choice [digit, char ' '])
        endOfLine
        return (nums ++ [' '])
    operatorParser = do
        op <-
            choice
                [ Multiply <$ char '*'
                , Add <$ char '+'
                ]

        spaces <- many1 space
        return (op, length spaces)

------------ TYPES ------------
data Operator = Add | Multiply deriving (Show, Eq)

type Input = [([Maybe Char], [Maybe Char], [Maybe Char], [Maybe Char], Operator)]

type OutputA = Int

type OutputB = Int

------------ PART 1 ------------
part1 :: Input -> OutputA
part1 = sum . map processRow
  where
    processRow (w, x, y, z, op) =
        let [w', x', y', z'] = map (read . catMaybes) [w, x, y, z]
         in case op of
                Add -> w' + x' + y' + z'
                Multiply -> w' * x' * y' * z'

------------ PART 2 ------------
part2 :: Input -> OutputB
part2 = sum . map perform
  where
    perform :: ([Maybe Char], [Maybe Char], [Maybe Char], [Maybe Char], Operator) -> Int
    perform (w, x, y, z, o) =
        let nums = map (read . catMaybes) $ transpose [w, x, y, z]
         in case o of
                Add -> sum nums
                Multiply -> product nums
