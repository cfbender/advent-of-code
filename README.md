# Advent of Code 

- [AoC 2024](https://adventofcode.com/2024/about)

- [AoC 2023](https://adventofcode.com/2023/about)

- [AoC 2022](https://adventofcode.com/2022/about)

- [AoC 2021](https://adventofcode.com/2021/about)

- [AoC 2020](https://adventofcode.com/2020/about)

Please don't judge any random language code here :weary:

### Dependencies
    - Deno
    - Rust (Rustup/Cargo)
    - Elixir (Mix/IEX)
    - Haskell (GHCUp)
    
### Running

##### Typescript
In respective day directory:
```bash
$ deno run --allow-read ./index.ts
```

##### Elixir (2020 only)
In respective day directory:
```bash
$ iex -S mix 
iex > Aoc.main()
```

##### Haskell
```bash
$ stack run -- -d <day number>
```

```bash
function day { ghcid --run="Main.performDay (Options (OneDay $1 Nothing) Timings)" }
```

```nushell
def day [day: int] { ghcid --run=$"Main.performDay \(Options \(OneDay ($day) Nothing) Timings)" }
```

##### Rust
In respective day directory:
```bash
$ cargo run
```

### Acknowledgements

- Thanks to [mhanberg for their wonderful starter repo](https://github.com/mhanberg/advent-of-code-elixir-starter)
- Thanks to [samcoy3 for their awesome haskell starter repo](https://github.com/samcoy3/advent-of-code-template)
