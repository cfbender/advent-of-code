defmodule AdventOfCode.Day20 do
  @moduledoc """
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(&String.to_integer/1)
  end

  def move(list, length, num, idx) do
    move = if num > length, do: num - length, else: num
    new_idx = rem(idx + move, length - 1)

    cond do
      # subtract an extra one because lists aren't zero indexed from the end
      new_idx < 0 -> new_idx - 1
      new_idx > 0 -> new_idx
      new_idx == 0 -> nil
    end
    |> case do
      nil ->
        List.delete_at(list, idx)
        |> List.insert_at(length, num)

      wrapped_idx ->
        List.delete_at(list, idx)
        |> List.insert_at(wrapped_idx, num)
    end
  end

  def find_indexes(list, num) do
    Enum.with_index(list)
    |> Enum.group_by(fn {x, _i} -> x end, fn {_x, i} -> i end)
    |> Map.get(num)

    # |> tap(fn _ -> dbg(length(list)) end)
  end

  def mix(list, length) do
    Enum.reduce(list, list, fn num, acc ->
      dbg(num)
      dbg(Enum.find_index(acc, &(&1 == num)))
      dbg(Enum.find_index(acc, &(&1 == 4363)))
      indexes = find_indexes(acc, num)

      Enum.reduce(indexes, acc, fn i, i_acc ->
        move(i_acc, length, num, i)
      end)
    end)
  end

  def part1(input) do
    # 20016 too high
    length = length(input) |> dbg()

    dbg(Enum.find_index(input, &(&1 == 4363)))
    result = mix(input, length)
    dbg(length(result))
    zero = Enum.find_index(result, &(&1 == 0))

    Enum.map([1000, 2000, 3000], fn x ->
      Enum.at(result, rem(zero + x, length))
    end)
    |> Enum.sum()
  end

  def part2(_args) do
  end
end
