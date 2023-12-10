module Benchmark where

import Criterion.Main
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
import Program.RunDay (Verbosity (Quiet))

main =
  defaultMain
    [ bench "Day 1" $ whnf (Day01.runDay Quiet) "Day01.txt"
    ]
