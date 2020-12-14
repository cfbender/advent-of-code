defmodule Aoc do
  def get_input do
    {:ok, input} = File.read("../input.txt")
    input |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
  end

  def reduce_pairs(lines, limit) do
    lines
    |> Enum.reduce(1, fn curr, acc ->
      if(curr < limit && Enum.member?(lines, limit - curr)) do
        acc * curr
      else
        acc
      end
    end)
  end

  def part1(lines) do
    reduce_pairs(lines, 2020)
  end

  def part2(lines) do
    lines
    |> Enum.reduce(0, fn curr, acc ->
      if(curr < 2020) do
        pair_quotient = reduce_pairs(lines, 2020 - curr)

        if(pair_quotient > 1) do
          curr * pair_quotient
        else
          acc
        end
      end
    end)
  end

  def main do
    lines = get_input()
    IO.puts("Part 1: #{part1(lines)}")
    IO.puts("Part 2: #{part2(lines)}")
  end

  def run1 do
    lines = get_input()
    part1(lines)
  end

  def run2 do
    lines = get_input()
    part2(lines)
  end
end
