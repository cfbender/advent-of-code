defmodule Mix.Tasks.D15.P2 do
  use Mix.Task

  import AdventOfCode.Day15

  @shortdoc "Day 15 Part 2"
  def run(args) do
    input =
      AdventOfCode.Input.get!(15, 2022)
      |> parse_input()

    if Enum.member?(args, "-b"),
      do: Benchee.run(%{part_2: fn -> input |> part2(4_000_000) end}),
      else:
        input
        |> part2(4_000_000)
        |> IO.inspect(label: "Part 2 Results")
  end
end
