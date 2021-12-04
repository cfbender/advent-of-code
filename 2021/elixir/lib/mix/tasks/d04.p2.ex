defmodule Mix.Tasks.D04.P2 do
  use Mix.Task

  import AdventOfCode.Day04

  @shortdoc "Day 04 Part 2"
  def run(args) do
    input =
      AdventOfCode.Input.get!(4, 2021)
      |> String.trim()
      |> String.split("\n\n", trim: true)
      |> Enum.map(fn line -> String.split(line, "\n") end)

    if Enum.member?(args, "-b"),
      do: Benchee.run(%{part_2: fn -> input |> part2() end}),
      else:
        input
        |> part2()
        |> IO.inspect(label: "Part 2 Results")
  end
end
