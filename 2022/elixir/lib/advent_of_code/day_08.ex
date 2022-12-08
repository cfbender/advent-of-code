defmodule AdventOfCode.Day08 do
  @moduledoc """
  Fun one! I think my thinking through the problem for part 1 paid off. Got part 2 quickly after 
  having all of the helpers and data structure I needed. 

  Got hung up on figuring out all of the deltas for neighbors, and how they differed in each direction.
  A quick refactor to clean up some of the mess I made and it's all good!

  Interestingly, I tried to use Task.await_many/2 here in order to parallelize all_views/3,
  but it seems to run significantly slower. Maybe an M1 thing?
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

  def view(map, range, key_map) do
    range
    |> Enum.filter(&(&1 != 0))
    |> Enum.reduce([], fn curr, acc ->
      key = key_map.(curr)
      neighbor = Map.get(map, key)
      [neighbor | acc]
    end)
  end

  # left and top don't need to be reversed because they start with the furthest away delta
  def left(map, {x, y}), do: view(map, -x..0, fn delta -> {x + delta, y} end)

  def right(map, {x, y}, max_x),
    do: view(map, 0..(max_x - x), fn delta -> {x + delta, y} end) |> Enum.reverse()

  def top(map, {x, y}), do: view(map, -y..0, fn delta -> {x, y + delta} end)

  def bottom(map, {x, y}, max_y),
    do: view(map, 0..(max_y - y), fn delta -> {x, y + delta} end) |> Enum.reverse()

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
      # individual expressions for lazy evaluation
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
