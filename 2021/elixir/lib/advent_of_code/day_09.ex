defmodule AdventOfCode.Day09 do
  def get_map(input) do
    input
    |> Enum.map(fn line ->
      String.split(line, "", trim: true) |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.with_index()
    |> Enum.reduce(Map.new(), fn {row, y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {val, x}, row_acc ->
        Map.put(row_acc, {x, y}, val)
      end)
    end)
  end

  def get_adj(map, {x, y}) do
    [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
    |> Enum.reduce(Map.new(), fn {dx, dy}, acc ->
      new_point = {x + dx, y + dy}

      Map.put(acc, new_point, Map.get(map, new_point))
    end)
  end

  def find_low_points(map) do
    Enum.filter(map, fn {point, val} ->
      get_adj(map, point)
      |> Enum.all?(fn {_point, adj_val} -> adj_val > val end)
    end)
    |> Map.new()
  end

  def find_basin(low_point, map, basin) do
    {point, value} = low_point

    new_basin_points =
      get_adj(map, point)
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
