def day [day: int] { 
  ghcid --command="stack ghci" --run=$"Main.performDay \(Options \(OneDay ($day) Nothing) Timings)" --target="aoc:exe:aoc-exe"
}

def bench [day: int] {
  let strDay = match $day {
    $x if $x < 10 => $"0($x)",
    _ => ($day | into string)
  }
  cabal bench --benchmark-options=$"day/($strDay)"
}

def test [day: int] {
  let strDay = match $day {
    $x if $x < 10 => $"0($x)",
    _ => ($day | into string)
  }
  stack test --ta $"--match \"Day ($strDay)\"" --file-watch
}

def edit [day: int] {
  let strDay = match $day {
    $x if $x < 10 => $"0($x)",
    _ => ($day | into string)
  }
  ^$env.EDITOR $"src/Days/Day($strDay).hs" $"input/Day($strDay).txt" $"test/Days/Day($strDay)Spec.hs"
}
