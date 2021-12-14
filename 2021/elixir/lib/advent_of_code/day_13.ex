defmodule AdventOfCode.Day13 do
  @fold_regex ~r/fold along ([x|y])=(\d+)/

  def fold(points, instruction) do
    [_, axis, line] = Regex.run(@fold_regex, instruction)

    fold_line = String.to_integer(line)

    Enum.map(points, fn
      {x, y} when x > fold_line and axis == "x" ->
        {2 * fold_line - x, y}

      {x, y} when y > fold_line and axis == "y" ->
        {x, 2 * fold_line - y}

      point ->
        point
    end)
    |> MapSet.new()
  end

  def print(code) do
    sorted_dots =
      MapSet.to_list(code)
      |> Enum.sort(fn {x1, y1}, {x2, y2} ->
        if y1 == y2 do
          x1 <= x2
        else
          y1 <= y2
        end
      end)

    {max_x, _} = Enum.max_by(sorted_dots, fn {x, _y} -> x end)
    {_, max_y} = Enum.max_by(sorted_dots, fn {_x, y} -> y end)

    IO.puts("\n")

    Enum.reduce(0..max_y, [], fn y, lines ->
      [
        Enum.map(0..max_x, fn x -> if {x, y} in sorted_dots, do: "#", else: " " end)
        |> Enum.join()
        | lines
      ]
    end)
    |> Enum.reverse()
    |> Enum.join("\n")
    |> IO.puts()

    IO.puts("\n")
  end

  def part1(input) do
    [coords, folds] = input

    Enum.map(coords, fn line ->
      String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
    end)
    |> fold(List.first(folds))
    |> MapSet.size()
  end

  def part2(input) do
    [coords, folds] = input

    map =
      Enum.map(coords, fn line ->
        String.split(line, ",") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      end)

    Enum.reduce(folds, map, fn e, acc -> fold(acc, e) end)
    |> print()
  end
end
