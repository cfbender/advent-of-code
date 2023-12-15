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
import Program.RunDay (Day, RunType (Both, Part1, Part2), Verbosity (None))
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
runDay :: Int -> RunType -> IO (Maybe Double, Maybe Double)
runDay i r = case days Map.!? i of
  Just (dayFunc, inputFile) -> dayFunc None inputFile r

main =
  defaultMain
    [ bgroup
        "day"
        [ bgroup
            "1"
            [ bench "1" $ whnfIO (runDay 1 Part1),
              bench "2" $ whnfIO (runDay 1 Part2)
            ],
          bgroup
            "2"
            [ bench "1" $ whnfIO (runDay 2 Part1),
              bench "2" $ whnfIO (runDay 2 Part2)
            ],
          bgroup
            "2"
            [ bench "1" $ whnfIO (runDay 2 Part1),
              bench "2" $ whnfIO (runDay 2 Part2)
            ],
          bgroup
            "3"
            [ bench "1" $ whnfIO (runDay 3 Part1),
              bench "2" $ whnfIO (runDay 3 Part2)
            ],
          bgroup
            "4"
            [ bench "1" $ whnfIO (runDay 4 Part1),
              bench "2" $ whnfIO (runDay 4 Part2)
            ],
          bgroup
            "5"
            [ bench "1" $ whnfIO (runDay 5 Part1),
              bench "2" $ whnfIO (runDay 5 Part2)
            ],
          bgroup
            "6"
            [ bench "1" $ whnfIO (runDay 6 Part1),
              bench "2" $ whnfIO (runDay 6 Part2)
            ],
          bgroup
            "7"
            [ bench "1" $ whnfIO (runDay 7 Part1),
              bench "2" $ whnfIO (runDay 7 Part2)
            ],
          bgroup
            "8"
            [ bench "1" $ whnfIO (runDay 8 Part1),
              bench "2" $ whnfIO (runDay 8 Part2)
            ],
          bgroup
            "9"
            [ bench "1" $ whnfIO (runDay 9 Part1),
              bench "2" $ whnfIO (runDay 9 Part2)
            ],
          bgroup
            "10"
            [ bench "1" $ whnfIO (runDay 10 Part1),
              bench "2" $ whnfIO (runDay 10 Part2)
            ],
          bgroup
            "11"
            [ bench "1" $ whnfIO (runDay 11 Part1),
              bench "2" $ whnfIO (runDay 11 Part2)
            ],
          bgroup
            "12"
            [ bench "1" $ whnfIO (runDay 12 Part1),
              bench "2" $ whnfIO (runDay 12 Part2)
            ],
          bgroup
            "13"
            [ bench "1" $ whnfIO (runDay 13 Part1),
              bench "2" $ whnfIO (runDay 13 Part2)
            ],
          bgroup
            "14"
            [ bench "1" $ whnfIO (runDay 14 Part1),
              bench "2" $ whnfIO (runDay 14 Part2)
            ],
          bgroup
            "15"
            [ bench "1" $ whnfIO (runDay 15 Part1),
              bench "2" $ whnfIO (runDay 15 Part2)
            ],
          bgroup
            "16"
            [ bench "1" $ whnfIO (runDay 16 Part1),
              bench "2" $ whnfIO (runDay 16 Part2)
            ],
          bgroup
            "17"
            [ bench "1" $ whnfIO (runDay 17 Part1),
              bench "2" $ whnfIO (runDay 17 Part2)
            ],
          bgroup
            "18"
            [ bench "1" $ whnfIO (runDay 18 Part1),
              bench "2" $ whnfIO (runDay 18 Part2)
            ],
          bgroup
            "19"
            [ bench "1" $ whnfIO (runDay 19 Part1),
              bench "2" $ whnfIO (runDay 19 Part2)
            ],
          bgroup
            "20"
            [ bench "1" $ whnfIO (runDay 20 Part1),
              bench "2" $ whnfIO (runDay 20 Part2)
            ],
          bgroup
            "21"
            [ bench "1" $ whnfIO (runDay 21 Part1),
              bench "2" $ whnfIO (runDay 21 Part2)
            ],
          bgroup
            "22"
            [ bench "1" $ whnfIO (runDay 22 Part1),
              bench "2" $ whnfIO (runDay 22 Part2)
            ],
          bgroup
            "23"
            [ bench "1" $ whnfIO (runDay 23 Part1),
              bench "2" $ whnfIO (runDay 23 Part2)
            ],
          bgroup
            "24"
            [ bench "1" $ whnfIO (runDay 24 Part1),
              bench "2" $ whnfIO (runDay 24 Part2)
            ],
          bgroup
            "25"
            [bench "1" $ whnfIO (runDay 25 Part1), bench "2" $ whnfIO (runDay 25 Part2)]
        ]
    ]
