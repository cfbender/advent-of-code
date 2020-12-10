defmodule AoC do
  def get_input do
    {:ok, input} = File.read("../input.txt")
    input |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
  end

  def part1(nums) do
    nums
    |> Enum.chunk_every(26, 1, :discard)
    |> Enum.find(fn x ->
      combos =
        for y <- x,
            z <- x,
            y != z,
            do:
              [y, z]
              |> Enum.sum()

      !Enum.member?(combos, List.last(x))
    end)
    |> List.last()
  end

  def list_check(list, check, i, full_list) do
    sum = Enum.sum(list)

    if sum == check do
      list
      |> Enum.min_max()
      |> Tuple.to_list()
      |> Enum.sum()
    else
      cond do
        sum < check ->
          list ++ [Enum.at(full_list, i + 1)]
          |> list_check(check, i + 1, full_list)
        true ->
          List.delete_at(list, 0)
          |> list_check(check, i, full_list)
      end
    end
  end

  def part2(nums, check) do
    nums
    |> List.first()
    |> List.wrap()
    |> list_check(check, 0, nums)
  end

  def main do
    input = get_input()
    result1 = part1(input)
    IO.puts("Part 1: #{result1}")
    result2 = part2(input, result1)
    IO.puts("Part 1: #{result2}")
  end

  def run1 do
    input = get_input()
    part1(input)
  end

  def run2 do
    input = get_input()
    result = part1(input)
    part2(input, result)
  end
end
