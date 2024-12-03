def day [day: int] { 
  ghcid --command="stack ghci" --run=$"Main.performDay \(Options \(OneDay ($day) Nothing) Timings)" --target="aoc:exe:aoc-exe"
}
