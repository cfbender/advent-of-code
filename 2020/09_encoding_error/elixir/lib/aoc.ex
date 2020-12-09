defmodule AoC do
  def get_input do
    {:ok, input} = File.read("../input.txt")
    input |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
  end

  def part1(nums) do
    nums 
    |> Enum.chunk_every(26, 1, :discard)
    |> Enum.find(fn x -> 
       combos = for  y <- x, z <- x, y != z, do: [y,z]
      |> Enum.sum()
      
      !Enum.member?(combos, List.last(x))
    end)
    |> List.last()
  end

  def part2(nums) do

  end

  def main do
    input = get_input()
    IO.puts "Part 1: #{part1(input)}"
  end

  def run1 do
    input = get_input()
    part1(input)
  end

  def run2 do
    input = get_input()
    part2(input)
  end
end
