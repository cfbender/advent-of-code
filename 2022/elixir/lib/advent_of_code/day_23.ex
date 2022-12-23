defmodule AdventOfCode.Day23 do
  @moduledoc """
  Fun one! Kinda tired of grids, and this sort of felt like a shade of Conway's game of life,
  but not too bad. At least it was a quick one, and almost for sure Christmas Eve's will be as well.

  Home stretch!
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(&String.codepoints/1)
    |> list_to_map()
    |> Stream.reject(&(elem(&1, 1) == "."))
    |> Stream.map(&put_elem(&1, 1, :elf))
    |> Enum.into(%{})
  end

  def occupied?({_x, y}, neighbors, :north),
    do: Enum.any?(neighbors, fn {{_nx, ny}, _} -> ny == y - 1 end)

  def occupied?({_x, y}, neighbors, :south),
    do: Enum.any?(neighbors, fn {{_nx, ny}, _} -> ny == y + 1 end)

  def occupied?({x, _y}, neighbors, :west),
    do: Enum.any?(neighbors, fn {{nx, _ny}, _} -> nx == x - 1 end)

  def occupied?({x, _y}, neighbors, :east),
    do: Enum.any?(neighbors, fn {{nx, _ny}, _} -> nx == x + 1 end)

  @proposals [:north, :south, :west, :east]
  def proposal(elves, elf, round) do
    neighbors = get_adj(elves, elf)

    [first, second, third, fourth] = Stream.cycle(@proposals) |> Enum.slice(round..(round + 3))

    cond do
      Enum.empty?(neighbors) -> nil
      not occupied?(elf, neighbors, first) -> move(elf, first)
      not occupied?(elf, neighbors, second) -> move(elf, second)
      not occupied?(elf, neighbors, third) -> move(elf, third)
      not occupied?(elf, neighbors, fourth) -> move(elf, fourth)
      true -> nil
    end
  end

  def move({x, y}, :north), do: {x, y - 1}
  def move({x, y}, :south), do: {x, y + 1}
  def move({x, y}, :east), do: {x + 1, y}
  def move({x, y}, :west), do: {x - 1, y}

  def run(elves, round \\ 0, limit \\ :infinity)
  def run(elves, round, limit) when round == limit, do: elves

  def run(elves, round, limit) do
    # map of elf position to proposed move
    all_moves =
      Enum.map(elves, fn {elf, _} -> {elf, proposal(elves, elf, round)} end)
      |> Enum.reject(fn {_, prop} -> is_nil(prop) end)

    next =
      Enum.reduce(all_moves, elves, fn {elf, move}, acc ->
        if Enum.count(all_moves, &(elem(&1, 1) == move)) > 1 do
          acc
        else
          Map.put(acc, move, :elf)
          |> Map.delete(elf)
        end
      end)

    if next == elves do
      round
    else
      run(next, round + 1, limit)
    end
  end

  def empty_tiles(map) do
    {{{_, y_min}, _}, {{_, y_max}, _}} = Enum.min_max_by(map, fn {{_, y}, _} -> y end)
    {{{x_min, _}, _}, {{x_max, _}, _}} = Enum.min_max_by(map, fn {{x, _}, _} -> x end)
    rows = y_max + abs(y_min) + 1
    cols = x_max + abs(x_min) + 1
    rows * cols - Enum.count(map)
  end

  def part1(input) do
    input
    |> run(0, 10)
    |> empty_tiles()
  end

  def part2(input) do
    input
    |> run(0)
    |> Kernel.+(1)
  end
end
