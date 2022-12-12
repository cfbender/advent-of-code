defmodule AdventOfCode.Day12 do
  @moduledoc """
  Snoozefest Dijkstras again! Just copy pasted day 15 last year and updated it to work with the
  character values. A little held up on some of that but easy enough once the bugs were squashed.

  Nice to refresh on Dijkstras again though, so not too mad.
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input =
      String.split(input, "\n", trim: true)
      |> Enum.map(fn line ->
        String.codepoints(line)
        |> Enum.map(fn char ->
          String.to_charlist(char) |> hd
        end)
      end)
      |> list_to_map()

    {start, _} = Enum.find(input, fn {_, v} -> v == ?S end)
    {dest, _} = Enum.find(input, fn {_, v} -> v == ?E end)

    {start, dest,
     Map.put(input, dest, ?z)
     |> Map.put(start, ?a)}
  end

  def get_cost(:infinity, _start, _next), do: :infinity
  def get_cost(cost, start, next), do: if(next - start <= 1, do: cost + 1, else: :infinity)

  def part1(input) do
    {start, dest, input} = input
    dijkstras(input, start, dest, &get_cost(&1, &2, &3))
  end

  def part2(input) do
    {_, dest, input} = input

    starts = Enum.filter(input, fn {_k, v} -> v == ?a end)

    Enum.map(starts, fn {point, _} -> dijkstras(input, point, dest, &get_cost(&1, &2, &3)) end)
    |> Enum.min()
  end
end
