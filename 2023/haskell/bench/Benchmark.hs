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
import Program.RunDay (Day, Verbosity (None))
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
runDay :: Int -> IO (Maybe Double, Maybe Double)
runDay i = case days Map.!? i of
  Just (dayFunc, inputFile) -> dayFunc None inputFile

main =
  defaultMain
    [ bgroup
        "day"
        [ bench "1." $ nfIO (runDay 1),
          bench "2." $ nfIO (runDay 2),
          bench "3." $ nfIO (runDay 3),
          bench "4." $ nfIO (runDay 4),
          bench "5." $ nfIO (runDay 5),
          bench "6." $ nfIO (runDay 6),
          bench "7." $ nfIO (runDay 7),
          bench "8." $ nfIO (runDay 8),
          bench "9." $ nfIO (runDay 9),
          bench "10." $ nfIO (runDay 10),
          bench "11." $ nfIO (runDay 11),
          bench "12." $ nfIO (runDay 12),
          bench "13." $ nfIO (runDay 13),
          bench "14." $ nfIO (runDay 14),
          bench "15." $ nfIO (runDay 15),
          bench "16." $ nfIO (runDay 16),
          bench "17." $ nfIO (runDay 17),
          bench "18." $ nfIO (runDay 18),
          bench "19." $ nfIO (runDay 19),
          bench "20." $ nfIO (runDay 20),
          bench "21." $ nfIO (runDay 21),
          bench "22." $ nfIO (runDay 22),
          bench "23." $ nfIO (runDay 23),
          bench "24." $ nfIO (runDay 24),
          bench "25." $ nfIO (runDay 25)
        ]
    ]
