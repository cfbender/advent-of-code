defmodule Aoc do
  def get_input do
    {:ok, input} = File.read("../input.txt")

    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x -> String.split(x, "", trim: true) end)
    |> Enum.with_index()
    |> Enum.reduce(Map.new(), fn {row, y}, acc ->
      row
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {symbol, x}, row_acc ->
        if symbol == "#", do: row_acc |> Map.put({x, y, 0, 0}, true), else: row_acc
      end)
    end)
  end

  @adj26 for(i <- [-1, 0, 1], j <- [-1, 0, 1], k <- [-1, 0, 1], do: {i, j, k})
         |> List.delete({0, 0, 0})

  @adj80 for(i <- [-1, 0, 1], j <- [-1, 0, 1], k <- [-1, 0, 1], l <- [-1, 0, 1], do: {i, j, k, l})
         |> List.delete({0, 0, 0, 0})

  def get_adj(map, point) do
    case point do
      {x, y, z} ->
        @adj26
        |> Enum.reduce(Map.new(), fn
          {dx, dy, dz}, acc ->
            new_point = {x + dx, y + dy, z + dz}

            acc
            |> Map.put(
              new_point,
              map |> Map.get(new_point)
            )
        end)

      {x, y, z, w} ->
        @adj80
        |> Enum.reduce(Map.new(), fn
          {dx, dy, dz, dw}, acc ->
            new_point = {x + dx, y + dy, z + dz, w + dw}

            acc
            |> Map.put(
              new_point,
              map |> Map.get(new_point)
            )
        end)
    end
  end

  def pad_map(map) do
    map
    |> Enum.reduce(map, fn {point, active}, acc ->
      if active, do: map |> get_adj(point) |> Map.merge(acc), else: acc
    end)
  end

  def process_state(map, 6), do: map

  def process_state(map, count) do
    map
    |> pad_map()
    |> Enum.reduce(map, fn {point, active}, acc ->
      neighbors =
        map
        |> get_adj(point)
        |> Enum.count(fn {_, active} -> active end)

      case active do
        true ->
          if neighbors == 2 or neighbors == 3,
            do: acc,
            else: acc |> Map.put(point, nil)

        _ ->
          acc |> Map.put(point, if(neighbors == 3, do: true, else: nil))
      end
    end)
    |> process_state(count + 1)
  end

  def main do
    input = get_input()

    part_1_input =
      input |> Enum.map(fn {{x, y, z, _w}, active} -> {{x, y, z}, active} end) |> Map.new()

    part_1 = process_state(part_1_input, 0) |> Enum.count(fn {_, x} -> !!x end)

    part_2 = process_state(input, 0) |> Enum.count(fn {_, x} -> !!x end)

    {part_1, part_2}
  end
end
