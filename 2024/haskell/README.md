# Advent of Code Haskell Template

<!--- advent_readme_stars table --->
## 2024 Results

| Day | Part 1 | Part 2 |
| :---: | :---: | :---: |
| [Day 1](https://adventofcode.com/2024/day/1) | ⭐ | ⭐ |
| [Day 2](https://adventofcode.com/2024/day/2) | ⭐ | ⭐ |
| [Day 3](https://adventofcode.com/2024/day/3) | ⭐ | ⭐ |
| [Day 4](https://adventofcode.com/2024/day/4) | ⭐ | ⭐ |
| [Day 5](https://adventofcode.com/2024/day/5) | ⭐ | ⭐ |
| [Day 6](https://adventofcode.com/2024/day/6) | ⭐ | ⭐ |
| [Day 7](https://adventofcode.com/2024/day/7) | ⭐ | ⭐ |
| [Day 8](https://adventofcode.com/2024/day/8) | ⭐ | ⭐ |
| [Day 9](https://adventofcode.com/2024/day/9) |  ⭐ | ⭐ |
| [Day 10](https://adventofcode.com/2024/day/10) |  ⭐ | ⭐ |
<!--- advent_readme_stars table --->
<!--- benchmarking table --->
## Benchmarks
<!--- fill in value. here's the units for handy reference: µs, ms, s --->
| Day | Part 1 | Part 2 |
| :---: | :---: | :---:  |
| [Day 1](./src/Days/Day01.hs) | `261.2 µs` | `259.7 µs` |
| [Day 2](./src/Days/Day02.hs) | `573.1 µs` | `541.1 µs` |
| [Day 3](./src/Days/Day03.hs) | `1.154 ms` | `1.128 ms` |
| [Day 4](./src/Days/Day04.hs) | `3.981 ms` | `3.940 ms` |
| [Day 5](./src/Days/Day05.hs) | `359.7 µs` | `365.4 µs` |
| [Day 6](./src/Days/Day06.hs) | `4.445 ms` | `27.52 s` |
| [Day 7](./src/Days/Day07.hs) | `938.7 µs` | `12.21 s` |
| [Day 8](./src/Days/Day08.hs) | `217.9 µs` | `213.6 µs` |
| [Day 9](./src/Days/Day09.hs) | `662.0 µs` | `631.2 µs` |
| [Day 10](./src/Days/Day10.hs) | `264.0 µs` | `208.3 µs` |

**Total: 39,750.1439 ms (12.8566 ms/37,737.2873 ms)**
<!--- benchmarking table --->

## Template

This is a fork of [samcoy3's haskell starter repo](https://github.com/samcoy3/advent-of-code-template) with some changes and scripts for my usage.

## Additional dependencies
- `regex-tdfa`: for regex operations just in case
- `split`: for string and list splitting
- `range`: for range operations
- `criterion`: for benchmarking
- `array`: when vector doesn't cut it 
- `parallel`: for parallelizing when you're bad at optimizing

## Tests

The repo now has a `test` directory where unit tests live. I like to put the sample input there and the expected result.

## Additional scripts and helpers

I've added some helper scripts that do common actions like `test.sh` `bench.sh` and `day.sh`

If using nushell, you can simply `source haskell.nu` and the scripts will be provided for you in the REPL as `test`, `bench` and `day`.

Run a day like `day 1` or run it's example tests like `test 1`.


## samcoy3's instructions (these should generally all hold true still but idk)

This is an (opinionated) Advent of Code template for solutions in Haskell.

To use:
- Clone this repository
- Set up a new branch for the year's solutions
- Change the package name, update the GitHub link, etc. You'll also want to remove the .cabal file and let stack generate a new one.
- Fill in the solutions and have fun!

When running from the command line you can pass the option `-d/--day DAY` to run a specific day's solutions. If you do this, then you can also pass `-i/--input FILE` to specify an input file; by default, the program will look for it in `input/DayXX.txt`. You can also pass the argument `--all-days` and all days will be run in order, assuming the input files are in their default places.

Additionally, you can specify the level of detail to print out. By default, the program will print only the answers. If you'd like it to print timing information, use the `-t/--timings` option. Alternatively, if you'd like it to print the output of the parser and error messages in full, use the `-v/--verbose` option.

Example usage:
- `stack run -- -d 9`: Runs Day 9's solutions.
- `stack run -- --day 14 --input "wibble.txt"`: Runs Day 14's solutions, using the input file "wibble.txt".
- `stack run -- -d 1 -i "alex.txt" --timings`: Runs Day 1's solutions, using the input file "alex.txt". Also prints timing information for each solution.
- `stack run -- --all-days`: Runs the solutions from all days.

This template can be used with `ghcid` to compile and run your code every time you save your files. Consider putting the following in your `.bashrc` (or equivalent):

```bash
function day { ghcid --run="Main.performDay (Options (OneDay $1 Nothing) Timings)" }
```

If sourced in a terminal, running the command `day 9`, for example, will, open a `ghcid` session and run your code every time you save, displaying the answers as if you ran the first example command above.

If you think the structure of the `Day` files needs changing to better suit your needs (before starting the project), then make the appropriate changes in `src/Days/Day01.hs` and run the `apply_changes.zsh` file. This will copy Day01 to all the other days, changing Day01 for DayXX as appropriate.

## Default Language Extensions

I've turned several language extensions on by default, including the set of stable and reasonable extensions implied by the `GHC2021` extension pack.
The other extensions enabled by default are:
- `GADTs`
- `LambdaCase`
- `MultiWayIf`
- `OverloadedRecordDot`
- `OverloadedStrings`
- `RecordWildCards`

The reason for these should be pretty clear in most cases.
If you want to change the default extensions, the list is in `package.yaml`.

## Default Dependencies

The default package dependencies for this project are:
- `directory`: This is just for checking if the provided input file exists.
- `time`: For timing the solutions.
- `ansi-term`: For colourful pretty printing.
- `attoparsec`: For the input parser for each day.
- `containers`: For Map, Set, and so on.
- `text`: Because `String`s are bad.
- `optparse-applicative`: For command line parsing.
- `mtl`: Mainly in anticipation that `State` might be useful. `ExceptT` is also used to catch exceptions in `runDay`.
- `vector`: In anticipation that fixed-length arrays will come in handy.
