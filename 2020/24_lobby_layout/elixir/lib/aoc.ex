defmodule Aoc do
  def get_input do
    input = File.read!("../input.txt")

    dir_regex = ~r"ne|e|se|sw|w|nw"

    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> dir_regex |> Regex.scan(line) |> List.flatten() end)
  end

  def movement(dir) do
    %{
      "n" => {0, 1},
      "ne" => {0.5, 0.5},
      "e" => {1, 0},
      "se" => {0.5, -0.5},
      "sw" => {-0.5, -0.5},
      "w" => {-1, 0},
      "nw" => {-0.5, 0.5}
    }
    |> Map.get(dir)
  end

  def find_pos(moves, start) do
    moves
    |> Enum.reduce(start, fn dir, {pos_x, pos_y} ->
      {dx, dy} = dir |> movement()
      {pos_x + dx, pos_y + dy}
    end)
  end

  @adj6 for(x <- [-1, 1], y <- [0], do: {x, y}) ++
          for(x <- [-0.5, 0.5], y <- [-0.5, 0.5], do: {x, y})

  def find_adj({pos_x, pos_y}) do
    @adj6
    |> Enum.reduce([], fn {dx, dy}, acc ->
      [{pos_x + dx, pos_y + dy} | acc]
    end)
  end

  def pad_map(map) do
    map
    |> Enum.reduce(map, fn {pos, _color}, acc ->
      pos |> find_adj() |> Map.new(fn pos -> {pos, 1} end) |> Map.merge(acc)
    end)
  end

  def flip_daily(floor, 100), do: floor

  def flip_daily(floor, count) do
    floor
    |> pad_map()
    |> Enum.reduce(floor, fn {pos, color}, acc ->
      adj =
        pos
        |> find_adj()
        |> Enum.map(fn adj_pos ->
          # get color of surrounding tiles, default to 1
          floor |> Map.get(adj_pos, 1)
        end)

      black = adj |> Enum.count(&(&1 == 0))

      new_color =
        case color do
          0 ->
            if black == 0 or black > 2, do: 1, else: 0

          1 ->
            if black == 2, do: 0, else: 1
        end

      acc |> Map.put(pos, new_color)
    end)
    |> flip_daily(count + 1)
  end

  def main do
    floor =
      get_input()
      |> Enum.reduce(Map.new(), fn inst, map ->
        pos = inst |> find_pos({0, 0})

        # 1 == white, default
        curr = map |> Map.get(pos, 1)

        map |> Map.put(pos, if(curr == 1, do: 0, else: 1))
      end)

    black_count =
      floor
      |> Enum.count(fn {_pos, val} -> val == 0 end)

    {black_count, floor |> flip_daily(0) |> Enum.count(fn {_pos, color} -> color == 0 end)}
  end
end
