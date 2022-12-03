defmodule AdventOfCode.Day03 do
  @moduledoc """
  First try'd the second part, felt great! Figured it was a set theory problem from the get-go. Previous years helped a ton here. Fun!
  """
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
  end

  @values for(
            x <- 0..25,
            y <- 0..25,
            do: [{Enum.at(?a..?z, x), x + 1}, {Enum.at(?A..?Z, y), y + 27}]
          )
          |> List.flatten()
          |> Enum.into(%{})

  defp charlist_set(str) do
    String.to_charlist(str)
    |> MapSet.new()
  end

  def part1(input) do
    input
    |> Enum.map(fn x ->
      String.split_at(
        x,
        String.length(x)
        |> div(2)
        |> floor()
      )
    end)
    |> Enum.map(fn {first, second} ->
      first = charlist_set(first)
      second = charlist_set(second)
      [overlap] = MapSet.intersection(first, second) |> MapSet.to_list()

      @values[overlap]
    end)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> Stream.chunk_every(3)
    |> Stream.map(fn group ->
      [badge] =
        Stream.map(group, fn sack ->
          String.to_charlist(sack)
          |> MapSet.new()
        end)
        |> Enum.reduce(&MapSet.intersection/2)
        |> MapSet.to_list()

      @values[badge]
    end)
    |> Enum.sum()
  end
end
