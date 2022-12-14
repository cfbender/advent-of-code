defmodule AdventOfCode.Day14 do
  @moduledoc """
  Pretty cool one! Got real stuck on how to bound and stop the sand,
  I kept having it stop in the last valid position, even if it would continue.

  Just had to delete it if it was going to move to an invalid spot next!
  """
  import AdventOfCode.Helpers

  # output shape: %{{494, 9} => :rock, {495, 9} => :rock, {496, 9} => :rock, ...}
  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(fn line ->
      String.split(line, " -> ")
      |> Enum.map(fn coord ->
        String.split(coord, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      end)
    end)
    |> Enum.reduce(%{}, fn line, acc ->
      Enum.chunk_every(line, 2, 1, :discard)
      |> Enum.reduce(acc, fn [{x1, y1}, {x2, y2}], l_acc ->
        if x1 == x2 do
          Enum.reduce(y1..y2, l_acc, fn y, y_acc ->
            Map.put(y_acc, {x1, y}, :rock)
          end)
          |> Map.merge(l_acc)
        else
          Enum.reduce(x1..x2, l_acc, fn x, x_acc ->
            Map.put(x_acc, {x, y1}, :rock)
          end)
          |> Map.merge(l_acc)
        end
      end)
    end)
  end

  @sand_start {500, 0}

  def down({x, y}), do: {x, y + 1}
  def down_left({x, y}), do: {x - 1, y + 1}
  def down_right({x, y}), do: {x + 1, y + 1}

  # functions return true values if slot is empty and valid
  def down_free?(map, sand),
    do: not Map.has_key?(map, down(sand))

  def down_left_free?(map, sand),
    do: not Map.has_key?(map, down_left(sand))

  def down_right_free?(map, sand),
    do: not Map.has_key?(map, down_right(sand))

  def resting?(map, sand) do
    not down_free?(map, sand) and not down_left_free?(map, sand) and
      not down_right_free?(map, sand)
  end

  def in_bounds?(map, {x, y}) do
    rocks = Enum.filter(map, fn {_, v} -> v == :rock end)
    {{{x_min, _}, _}, {{x_max, _}, _}} = Enum.min_max_by(rocks, fn {{x, _}, _} -> x end)
    {{_, y_max}, _} = Enum.max_by(rocks, fn {{_, y}, _} -> y end)

    x in x_min..x_max and y < y_max
  end

  def add_sand(map), do: Map.put(map, @sand_start, :sand)

  def move_sand(map, {_, y} = sand, floor) do
    # sets the floor for part 2
    valid = if floor, do: y < floor, else: in_bounds?(map, sand)

    if resting?(map, sand) or not valid do
      map
    else
      new =
        cond do
          down_free?(map, sand) -> down(sand)
          down_left_free?(map, sand) -> down_left(sand)
          down_right_free?(map, sand) -> down_right(sand)
        end

      # has a valid move out of bounds, delete it and return
      if is_nil(floor) and not in_bounds?(map, new) do
        Map.delete(map, sand)
      else
        # move sand to new position and recurse
        Map.put(map, new, :sand) |> Map.delete(sand) |> move_sand(new, floor)
      end
    end
  end

  def new_sand(map, floor) do
    map = add_sand(map)
    move_sand(map, @sand_start, floor)
  end

  def pour(input, floor \\ nil) do
    added = new_sand(input, floor)

    if added == input, do: input, else: pour(added, floor)
  end

  def part1(input) do
    pour(input)
    |> Enum.count(fn {_k, v} -> v == :sand end)
  end

  def part2(input) do
    floor =
      (Enum.map(input, fn {{_x, y}, _} ->
         y
       end)
       |> Enum.max()) + 1

    pour(input, floor)
    |> Enum.count(fn {_k, v} -> v == :sand end)
  end
end
