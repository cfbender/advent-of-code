defmodule AdventOfCode.Day02 do
  def part1(input) do
    Stream.map(input, fn str -> String.split(str, " ") end)
    |> Stream.map(fn [type, x] -> [type, String.to_integer(x)] end)
    |> Enum.reduce(%{depth: 0, pos: 0}, fn [type, x], %{depth: depth, pos: pos} ->
      case type do
        "forward" ->
          %{depth: depth, pos: pos + x}

        "down" ->
          %{depth: depth + x, pos: pos}

        "up" ->
          %{depth: depth - x, pos: pos}
      end
    end)
    |> Map.values()
    |> Enum.product()
  end

  def part2(input) do
    %{depth: depth, pos: pos} =
      Stream.map(input, fn str -> String.split(str, " ") end)
      |> Stream.map(fn [type, x] -> [type, String.to_integer(x)] end)
      |> Enum.reduce(%{depth: 0, pos: 0, aim: 0}, fn [type, x],
                                                     %{depth: depth, pos: pos, aim: aim} ->
        case type do
          "forward" ->
            %{depth: depth + aim * x, pos: pos + x, aim: aim}

          "down" ->
            %{depth: depth, pos: pos, aim: aim + x}

          "up" ->
            %{depth: depth, pos: pos, aim: aim - x}
        end
      end)

    depth * pos
  end
end
