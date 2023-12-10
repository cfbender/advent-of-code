import Criterion.Main
import Data.Map (Map)
import Data.Map qualified as Map
import Days.Day01 qualified as Day01
import Days.Day02 qualified as Day02
import Days.Day03 qualified as Day03
import Days.Day04 qualified as Day04
import Days.Day05 qualified as Day05
import Days.Day06 qualified as Day06
import Days.Day07 qualified as Day07
import Days.Day08 qualified as Day08
import Days.Day09 qualified as Day09
import Days.Day10 qualified as Day10
import Days.Day11 qualified as Day11
import Days.Day12 qualified as Day12
import Days.Day13 qualified as Day13
import Days.Day14 qualified as Day14
import Days.Day15 qualified as Day15
import Days.Day16 qualified as Day16
import Days.Day17 qualified as Day17
import Days.Day18 qualified as Day18
import Days.Day19 qualified as Day19
import Days.Day20 qualified as Day20
import Days.Day21 qualified as Day21
import Days.Day22 qualified as Day22
import Days.Day23 qualified as Day23
import Days.Day24 qualified as Day24
import Days.Day25 qualified as Day25
import Program.Color (withColor)
import Program.RunDay (Day, Verbosity (Quiet))
import System.Console.ANSI (Color (..))
import Text.Printf (printf)

days =
  Map.fromList . zip [1 ..] $
    [ (Day01.runDay, "input/Day01.txt"),
      (Day02.runDay, "input/Day02.txt"),
      (Day03.runDay, "input/Day03.txt"),
      (Day04.runDay, "input/Day04.txt"),
      (Day05.runDay, "input/Day05.txt"),
      (Day06.runDay, "input/Day06.txt"),
      (Day07.runDay, "input/Day07.txt"),
      (Day08.runDay, "input/Day08.txt"),
      (Day09.runDay, "input/Day09.txt"),
      (Day10.runDay, "input/Day10.txt"),
      (Day11.runDay, "input/Day11.txt"),
      (Day12.runDay, "input/Day12.txt"),
      (Day13.runDay, "input/Day13.txt"),
      (Day14.runDay, "input/Day14.txt"),
      (Day15.runDay, "input/Day15.txt"),
      (Day16.runDay, "input/Day16.txt"),
      (Day17.runDay, "input/Day17.txt"),
      (Day18.runDay, "input/Day18.txt"),
      (Day19.runDay, "input/Day19.txt"),
      (Day20.runDay, "input/Day20.txt"),
      (Day21.runDay, "input/Day21.txt"),
      (Day22.runDay, "input/Day22.txt"),
      (Day23.runDay, "input/Day23.txt"),
      (Day24.runDay, "input/Day24.txt"),
      (Day25.runDay, "input/Day25.txt")
    ]

days :: Map Int (Day, String)

runDay i = case days Map.!? i of
  Nothing -> putStrLn "Invalid day provided. There are 25 days in Advent."
  Just (dayFunc, inputFile) -> do
    withColor Magenta $ putStrLn $ printf "\n***Day %02d***" i
    dayFunc Quiet inputFile
    withColor Magenta $ putStrLn "************"

main =
  defaultMain
    [ bgroup
        "day"
        [ bench "1" $ whnf runDay 1,
          bench "2" $ whnf runDay 2,
          bench "3" $ whnf runDay 3,
          bench "4" $ whnf runDay 4,
          bench "5" $ whnf runDay 5,
          bench "6" $ whnf runDay 6,
          bench "7" $ whnf runDay 7,
          bench "8" $ whnf runDay 8,
          bench "9" $ whnf runDay 9,
          bench "10" $ whnf runDay 10,
          bench "11" $ whnf runDay 11,
          bench "12" $ whnf runDay 12,
          bench "13" $ whnf runDay 13,
          bench "14" $ whnf runDay 14,
          bench "15" $ whnf runDay 15,
          bench "16" $ whnf runDay 16,
          bench "17" $ whnf runDay 17,
          bench "18" $ whnf runDay 18,
          bench "19" $ whnf runDay 19,
          bench "20" $ whnf runDay 20,
          bench "21" $ whnf runDay 21,
          bench "22" $ whnf runDay 22,
          bench "23" $ whnf runDay 23,
          bench "24" $ whnf runDay 24,
          bench "25" $ whnf runDay 25
        ]
    ]
