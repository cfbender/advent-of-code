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
import Program.Color (withColor)
import Program.RunDay (Day, RunType (Both, Part1, Part2), Verbosity (None))
import System.Console.ANSI (Color (..))
import Text.Printf (printf)

days =
  Map.fromList . zip [1 ..] $
    [ (Day01.runDay, "input/Day01.txt")
    , (Day02.runDay, "input/Day02.txt")
    , (Day03.runDay, "input/Day03.txt")
    , (Day04.runDay, "input/Day04.txt")
    , (Day05.runDay, "input/Day05.txt")
    , (Day06.runDay, "input/Day06.txt")
    , (Day07.runDay, "input/Day07.txt")
    , (Day08.runDay, "input/Day08.txt")
    , (Day09.runDay, "input/Day09.txt")
    , (Day10.runDay, "input/Day10.txt")
    , (Day11.runDay, "input/Day11.txt")
    , (Day12.runDay, "input/Day12.txt")
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
            [ bench "1" $ whnfIO (runDay 1 Part1)
            , bench "2" $ whnfIO (runDay 1 Part2)
            ]
        , bgroup
            "2"
            [ bench "1" $ whnfIO (runDay 2 Part1)
            , bench "2" $ whnfIO (runDay 2 Part2)
            ]
        , bgroup
            "2"
            [ bench "1" $ whnfIO (runDay 2 Part1)
            , bench "2" $ whnfIO (runDay 2 Part2)
            ]
        , bgroup
            "3"
            [ bench "1" $ whnfIO (runDay 3 Part1)
            , bench "2" $ whnfIO (runDay 3 Part2)
            ]
        , bgroup
            "4"
            [ bench "1" $ whnfIO (runDay 4 Part1)
            , bench "2" $ whnfIO (runDay 4 Part2)
            ]
        , bgroup
            "5"
            [ bench "1" $ whnfIO (runDay 5 Part1)
            , bench "2" $ whnfIO (runDay 5 Part2)
            ]
        , bgroup
            "6"
            [ bench "1" $ whnfIO (runDay 6 Part1)
            , bench "2" $ whnfIO (runDay 6 Part2)
            ]
        , bgroup
            "7"
            [ bench "1" $ whnfIO (runDay 7 Part1)
            , bench "2" $ whnfIO (runDay 7 Part2)
            ]
        , bgroup
            "8"
            [ bench "1" $ whnfIO (runDay 8 Part1)
            , bench "2" $ whnfIO (runDay 8 Part2)
            ]
        , bgroup
            "9"
            [ bench "1" $ whnfIO (runDay 9 Part1)
            , bench "2" $ whnfIO (runDay 9 Part2)
            ]
        , bgroup
            "10"
            [ bench "1" $ whnfIO (runDay 10 Part1)
            , bench "2" $ whnfIO (runDay 10 Part2)
            ]
        , bgroup
            "11"
            [ bench "1" $ whnfIO (runDay 11 Part1)
            , bench "2" $ whnfIO (runDay 11 Part2)
            ]
        , bgroup
            "12"
            [ bench "1" $ whnfIO (runDay 12 Part1)
            , bench "2" $ whnfIO (runDay 12 Part2)
            ]
        ]
    ]
