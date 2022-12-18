defmodule AdventOfCode.Day18 do
  @moduledoc """
  I liked this one! Just the right amount of difficulty, part 2 really made me think.
  Feel like I could write this a lot cleaner though, way too much fiddly stuff in here.

  Thanks to @caleboleary for the test input and answer, the test input given on AoC was completely useless
  for actually getting part 2, which is frustrating.
  """

  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(fn line ->
      {String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple(), :lava}
    end)
    |> Map.new()
  end

  def droplet_bounds(map) do
    {{{x_min, _, _}, _}, {{x_max, _, _}, _}} = Enum.min_max_by(map, fn {{x, _, _}, _} -> x end)
    {{{_, y_min, _}, _}, {{_, y_max, _}, _}} = Enum.min_max_by(map, fn {{_, y, _}, _} -> y end)
    {{{_, _, z_min}, _}, {{_, _, z_max}, _}} = Enum.min_max_by(map, fn {{_, _, z}, _} -> z end)

    for(x <- x_min..x_max, y <- y_min..y_max, z <- z_min..z_max, do: {x, y, z})
    |> MapSet.new()
  end

  def check_paths(map, bounds, point, checked) do
    open_neighbors =
      get_adj(map, point, three_d: true, all: false, default: :air)
      |> Enum.reject(fn {_, v} -> v == :lava end)
      |> MapSet.new()
      |> MapSet.difference(checked)

    cond do
      not MapSet.member?(bounds, point) ->
        true

      MapSet.size(open_neighbors) == 0 ->
        false

      true ->
        new_checked = MapSet.union(open_neighbors, checked) |> MapSet.put(point)

        Enum.reduce_while(open_neighbors, {new_checked, true}, fn {neighbor, _}, {checked, val} ->
          case check_paths(map, bounds, neighbor, checked) do
            true ->
              {:halt, true}

            false ->
              {:cont, {MapSet.put(checked, neighbor), val}}

            {nc, v} ->
              {:cont, {MapSet.union(checked, nc), val || v}}
          end
        end)
    end
  end

  def open?(map, bounds, point) do
    case check_paths(map, bounds, point, MapSet.new()) do
      {_, _} -> false
      v -> v
    end
  end

  def scan_droplet(map) do
    z_slices = Enum.group_by(map, fn {{_, _, pz}, _} -> pz end)
    bounds = droplet_bounds(map)

    for {z, z_slice} <- z_slices do
      y_slices = Enum.group_by(z_slice, fn {{_, py, _}, _} -> py end)

      for {y, y_slice} <- y_slices do
        {{{x_min, _, _}, _}, {{x_max, _, _}, _}} =
          Enum.min_max_by(y_slice, fn {{x, _, _}, _} -> x end)

        for x <- x_min..x_max do
          point = {x, y, z}

          if not Map.has_key?(map, point) and open?(map, bounds, point) do
            {{x, y, z}, :open}
          else
            {{x, y, z}, :lava}
          end
        end
      end
    end
    |> List.flatten()
    |> Enum.into(%{})
  end

  def all_surface_area(map) do
    Enum.reduce(map, 0, fn {point, v}, sum ->
      if v == :open do
        sum
      else
        neighbors = get_adj(map, point, three_d: true, all: false) |> Map.keys()
        hidden = Enum.count(neighbors, fn p -> map[p] == :lava end)
        sum + (6 - hidden)
      end
    end)
  end

  def part1(input) do
    all_surface_area(input)
  end

  def part2(input) do
    scan_droplet(input)
    |> all_surface_area()
  end
end
