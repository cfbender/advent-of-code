defmodule AoC do
  @spec get_grid :: [[String.t()]]
  def get_grid() do
    {:ok, input} = File.read("../input.txt")

    lines =
      input
      |> String.split(~r/\r?\n/, trim: true)
      |> Enum.map(fn x -> ("*" <> x <> "*") |> String.split("") end)

    padding = List.duplicate("*", lines |> Enum.at(0) |> Enum.count())
    [padding | lines] ++ [padding]
  end

  @adj8 for(i <- [-1, 0, 1], j <- [-1, 0, 1], do: {i, j}) |> List.delete({0, 0})

  def get_next_seat(seats, {dx, dy}, {x, y}) do
    next_x = x + dx
    next_y = y + dy

    if seats |> Enum.at(next_x) |> Enum.at(next_y) == ".",
      do: get_next_seat(seats, {dx, dy}, {next_x, next_y}),
      else: {next_x, next_y}
  end

  def get_sight(seats) do
    seats
    |> Enum.with_index()
    |> Enum.map(fn {row, i} ->
      row
      |> Enum.with_index()
      |> Enum.map(fn
        {seat, j} ->
          cond do
            seat != "*" ->
              Enum.map(@adj8, fn dir ->
                get_next_seat(seats, dir, {i, j})
              end)

            true ->
              nil
          end
      end)
      |> Enum.reject(fn x -> x == nil end)
    end)
  end

  def process(part, seats, sight) do
    new =
      seats
      |> Enum.with_index()
      |> Enum.map(fn {row, i} ->
        row
        |> Enum.with_index()
        |> Enum.map(fn {seat, j} ->
          case seat do
            x when x in ["#", "L"] ->
              num =
                if(part == 1, do: @adj8, else: sight |> Enum.at(i) |> Enum.at(j))
                |> Enum.count(fn {dx, dy} ->
                  row = seats |> Enum.at(i + dx)
                  row && row |> Enum.at(j + dy) == "#"
                end)

              if seat == "L",
                do:
                  if(num == 0,
                    do: "#",
                    else: "L"
                  ),
                else: if(num >= if(part == 1, do: 4, else: 5), do: "L", else: "#")

            _ ->
              seat
          end
        end)
      end)

    if new == seats,
      do:
        new
        |> Enum.reduce(0, fn row, acc -> acc + Enum.count(row, fn x -> x == "#" end) end),
      else: process(part, new, sight)
  end

  def main do
    seats = get_grid()
    sight = get_sight(seats)

    result1 = process(1, seats, sight)
    IO.puts("Part 1: #{result1}")
    result2 = process(2, seats, sight)
    IO.puts("Part 1: #{result2}")

    {result1, result2}
  end
end
