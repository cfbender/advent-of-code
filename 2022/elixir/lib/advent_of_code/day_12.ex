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

  def dijkstras(map, start, dest) do
    dijkstras([{start, 0}], map, dest, Map.new(Enum.map(Map.keys(map), &{&1, :infinity})))
  end

  def dijkstras([], _map, _dest, _costs), do: :infinity

  def dijkstras(queue, map, dest, costs) do
    [{node, cost} | rest_queue] = queue

    if node == dest do
      cost
    else
      neighbors = get_adj(map, node, all: false) |> Map.keys()

      # add neighbor with added weight to queue
      {new_queue, new_costs} =
        Enum.reduce(neighbors, {rest_queue, costs}, fn neighbor, {q_acc, c_acc} = acc ->
          current_cost = Map.get(costs, neighbor)
          start = Map.get(map, node)
          next = Map.get(map, neighbor)
          new_cost = get_cost(cost, start, next)

          if new_cost < current_cost do
            {[{neighbor, new_cost} | q_acc], Map.put(c_acc, neighbor, new_cost)}
          else
            acc
          end
        end)

      dijkstras(Enum.sort(new_queue), map, dest, new_costs)
    end
  end

  def part1(input) do
    {start, dest, input} = input

    dijkstras(input, start, dest)
  end

  def part2(input) do
    {_, dest, input} = input

    starts = Enum.filter(input, fn {_k, v} -> v == ?a end)

    Enum.map(starts, fn {point, _} -> dijkstras(input, point, dest) end)
    |> Enum.min()
  end
end
