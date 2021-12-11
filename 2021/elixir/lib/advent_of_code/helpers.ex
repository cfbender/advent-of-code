defmodule AdventOfCode.Helpers do
  @doc """
  Converts a 2D list to a map of coordinates with the given value

  ## Examples

      iex> Helpers.list_to_map([[1,2,3], [4,5,6]])
      %{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}
  """
  def list_to_map(list) do
    Enum.with_index(list)
    |> Enum.reduce(Map.new(), fn {row, y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {val, x}, row_acc ->
        Map.put(row_acc, {x, y}, val)
      end)
    end)
  end

  @doc """
  Gets all adjacent points on a map of XY coordinates to a point

  Pass `all: false` to not include diagonally adjacent points

  ## Examples
      iex> Helpers.get_adj(%{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}, {1,1})
      %{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}
  """
  def get_adj(map, {x, y}, [all: all] \\ [all: true]) do
    deltas =
      if all,
        do: for(x <- [-1, 0, 1], y <- [-1, 0, 1], do: {x, y}),
        else: [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]

    deltas
    |> Enum.reduce(Map.new(), fn {dx, dy}, acc ->
      new_point = {x + dx, y + dy}

      new_point_val = Map.get(map, new_point)

      if new_point_val do
        Map.put(acc, new_point, new_point_val)
      else
        acc
      end
    end)
  end
end
