#! /bin/bash

ghcid --command="stack ghci" --run="Main.performDay (Options (OneDay $1 Nothing) Timings)" --target="aoc:exe:aoc-exe"
