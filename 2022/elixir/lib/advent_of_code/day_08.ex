defmodule AdventOfCode.Day08 do
  @moduledoc """
  """
  def parse_input(input) do
    lines =
      input
      |> String.split("\n", trim: true)
      |> Enum.with_index()

    {_, max_y} = Enum.max_by(lines, &elem(&1, 1))

    trees =
      Enum.map(lines, fn {line, row} ->
        String.graphemes(line)
        |> Enum.with_index()
        |> Enum.map(fn {tree, col} ->
          {{col, row}, String.to_integer(tree)}
        end)
      end)
      |> List.flatten()

    {{max_x, _}, _} = Enum.max_by(trees, fn {{x, _y}, _} -> x end)

    {max_x, max_y, Enum.into(trees, %{})}
  end

  def left(map, {x, y}) do
    -x..0
    |> Enum.filter(&(&1 != 0))
    |> Enum.reduce([], fn curr, acc ->
      key = {x + curr, y}
      neighbor = Map.get(map, key)
      [neighbor | acc]
    end)
  end

  def right(map, {x, y}, max_x) do
    0..(max_x - x)
    |> Enum.filter(&(&1 != 0))
    |> Enum.reduce([], fn curr, acc ->
      key = {x + curr, y}
      neighbor = Map.get(map, key)
      [neighbor | acc]
    end)
    |> Enum.reverse()
  end

  def top(map, {x, y}) do
    -y..0
    |> Enum.filter(&(&1 != 0))
    |> Enum.reduce([], fn curr, acc ->
      key = {x, curr + y}
      neighbor = Map.get(map, key)
      [neighbor | acc]
    end)
  end

  def bottom(map, {x, y}, max_y) do
    0..(max_y - y)
    |> Enum.filter(&(&1 != 0))
    |> Enum.reduce([], fn curr, acc ->
      key = {x, curr + y}
      neighbor = Map.get(map, key)
      [neighbor | acc]
    end)
    |> Enum.reverse()
  end

  def visible?(height, list) do
    Enum.all?(list, fn other_height -> height > other_height end)
  end

  def viewing_distance(height, view) do
    Enum.reduce_while(view, 0, fn other_height, acc ->
      if height > other_height do
        {:cont, acc + 1}
      else
        {:halt, acc + 1}
      end
    end)
  end

  def all_views(trees, coords, {max_x, max_y}) do
    [
      top(trees, coords),
      bottom(trees, coords, max_y),
      left(trees, coords),
      right(trees, coords, max_x)
    ]
  end

  def part1(input) do
    {max_x, max_y, trees} = input

    Enum.count(trees, fn {coords, height} ->
      visible?(height, top(trees, coords)) ||
        visible?(height, bottom(trees, coords, max_y)) ||
        visible?(height, left(trees, coords)) ||
        visible?(height, right(trees, coords, max_x))
    end)
  end

  def part2(input) do
    {max_x, max_y, trees} = input

    Enum.map(trees, fn {coords, height} ->
      all_views(trees, coords, {max_x, max_y})
      |> Enum.map(fn views -> viewing_distance(height, views) end)
      |> Enum.product()
    end)
    |> Enum.max()
  end
end
