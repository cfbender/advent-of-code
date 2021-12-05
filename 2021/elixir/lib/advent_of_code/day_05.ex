defmodule AdventOfCode.Day05 do
  def get_coords(input) do
    input
    |> Enum.map(fn line ->
      String.split(line, " -> ", trim: true)
      |> Enum.map(fn coord ->
        [x, y] = String.split(coord, ",")
        {String.to_integer(x), String.to_integer(y)}
      end)
    end)
  end

  def get_lines(coords, diagonals \\ false) do
    Enum.map(coords, fn [{x1, y1}, {x2, y2}] ->
      if x1 == x2 or y1 == y2 do
        for x <- x1..x2, y <- y1..y2, do: {x, y}
      else
        # maybe a prettier way to do this in one comprehension?
        if diagonals do
          Enum.zip(
            for(x <- x1..x2, do: x),
            for(y <- y1..y2, do: y)
          )
        end
      end
    end)
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
  end

  def build_grid(lines) do
    Enum.reduce(lines, Map.new(), fn pos, acc ->
      Map.update(acc, pos, 1, &(&1 + 1))
    end)
  end

  def part1(input) do
    get_coords(input)
    |> get_lines()
    |> build_grid()
    |> Enum.count(fn {_coord, count} -> count > 1 end)
  end

  def part2(input) do
    get_coords(input)
    |> get_lines(true)
    |> build_grid()
    |> Enum.count(fn {_coord, count} -> count > 1 end)
  end
end
