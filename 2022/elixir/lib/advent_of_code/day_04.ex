defmodule AdventOfCode.Day04 do
  @moduledoc """
  This was probably the closest I'll ever get to the global leaderboard 😂, got both in 10 minutes.
  Spent a little bit deciding if I should keep it a range, but MapSet it is again! 
  """
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      String.split(line, ",")
      |> Enum.map(fn assign ->
        [start, ending] =
          String.split(assign, "-")
          |> Enum.map(&String.to_integer/1)

        start..ending |> MapSet.new()
      end)
    end)
  end

  def part1(input) do
    input
    |> Stream.filter(fn [set1, set2] ->
      MapSet.subset?(set1, set2) || MapSet.subset?(set2, set1)
    end)
    |> Enum.count()
  end

  def part2(input) do
    input
    |> Stream.filter(fn [set1, set2] ->
      not MapSet.disjoint?(set1, set2)
    end)
    |> Enum.count()
  end
end
