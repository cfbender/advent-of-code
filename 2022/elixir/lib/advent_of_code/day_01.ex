defmodule AdventOfCode.Day01 do
  @moduledoc """
  Easy day 1 as usual! Part 2 is obviously inefficient for larger data sets, but thems the rules of AoC. If it ain't a large data set by part 2, YOLO
  """
  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn x -> String.split(x, "\n") |> Enum.map(&String.to_integer/1) end)
  end

  def part1(input) do
    input |> Enum.map(&Enum.sum/1) |> Enum.max()
  end

  def part2(input) do
    input |> Enum.map(&Enum.sum/1) |> Enum.sort(&(&1 >= &2)) |> Enum.take(3) |> Enum.sum()
  end
end
