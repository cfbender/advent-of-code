defmodule Mix.Tasks.D18.P1 do
  use Mix.Task

  import AdventOfCode.Day18

  @shortdoc "Day 18 Part 1"
  def run(args) do
    input =
      AdventOfCode.Input.get!(18, 2022)
      |> parse_input()

    if Enum.member?(args, "-b"),
      do: Benchee.run(%{part_1: fn -> input |> part1() end}),
      else:
        input
        |> part1()
        |> IO.inspect(label: "Part 1 Results")
  end
end
