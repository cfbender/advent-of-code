defmodule AdventOfCode.Day09 do
  alias AdventOfCode.Helpers

  def get_map(input) do
    input
    |> Enum.map(fn line ->
      String.codepoints(line) |> Enum.map(&String.to_integer/1)
    end)
    |> Helpers.list_to_map()
  end

  def find_low_points(map) do
    Enum.filter(map, fn {point, val} ->
      Helpers.get_adj(map, point, all: false)
      |> Enum.all?(fn {_point, adj_val} -> adj_val > val end)
    end)
    |> Map.new()
  end

  def find_basin(low_point, map, basin) do
    {point, value} = low_point

    new_basin_points =
      Helpers.get_adj(map, point, all: false)
      |> Enum.filter(fn {_point, adj_val} ->
        adj_val > value and adj_val != 9 and not is_nil(adj_val)
      end)

    new_basin = MapSet.union(basin, MapSet.new(new_basin_points))

    MapSet.union(
      new_basin,
      # find basin surrounding all new basin points
      Enum.flat_map(new_basin_points, fn point -> find_basin(point, map, new_basin) end)
      |> MapSet.new()
    )
  end

  def part1(input) do
    get_map(input)
    |> find_low_points()
    |> Map.values()
    |> Enum.map(&(&1 + 1))
    |> Enum.sum()
  end

  def part2(input) do
    map = get_map(input)

    find_low_points(map)
    |> Stream.map(fn low_point -> find_basin(low_point, map, MapSet.new([low_point])) end)
    |> Stream.map(&MapSet.size/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end
end
