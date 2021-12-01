defmodule AdventOfCode.Day01 do
  def part1(input) do
    Enum.chunk_every(input, 2, 1, :discard) |> Enum.count(fn [x, y] -> y - x > 0 end)
  end

  def part2(input) do
    Enum.chunk_every(input, 3, 1, :discard) |> Enum.map(&Enum.sum/1) |> part1()
  end
end
