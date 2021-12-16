defmodule AdventOfCode.Day15 do
  import AdventOfCode.Helpers

  @source {0, 0}

  @moduledoc """
  Note: Using just a list instead of a priority queue just to not use a library.
  A little under-optimized, and not using the Helpers.Graph data structure
  since it's a little slow as well
  """

  def dijkstras(map, dest) do
    dijkstras([{@source, 0}], map, dest, Map.new(Enum.map(Map.keys(map), &{&1, Infinity})))
  end

  def dijkstras(queue, map, dest, costs) do
    [{node, cost} | rest_queue] = queue

    if node == dest do
      cost
    else
      neighbors = get_adj(map, node, all: false) |> Map.keys()

      # add neighbor with added weight to queue
      {new_queue, new_costs} =
        Enum.reduce(neighbors, {rest_queue, costs}, fn point, {q_acc, c_acc} = acc ->
          new_cost = cost + Map.get(map, point)

          if new_cost < Map.get(costs, point) do
            {[{point, new_cost} | q_acc], Map.put(c_acc, point, new_cost)}
          else
            acc
          end
        end)

      dijkstras(Enum.sort(new_queue), map, dest, new_costs)
    end
  end

  def process_input(input),
    do: Enum.map(input, fn line -> String.codepoints(line) |> Enum.map(&String.to_integer/1) end)

  def part1(input) do
    map =
      input
      |> list_to_map()

    points = Map.keys(map)

    destination = Enum.max(points)

    dijkstras(map, destination)
  end

  def transform_input(input, level) do
    cycle = Stream.cycle(1..9)

    input
    |> process_input()
    |> Enum.map(fn line ->
      Enum.map(line, fn x ->
        Enum.at(cycle, x + level - 1)
      end)
    end)
  end

  def part2(input) do
    levels =
      for x <- 0..4,
          y <- 0..4,
          do: x + y

    Enum.chunk_every(levels, 5)
    |> Enum.flat_map(fn line ->
      Enum.reduce(line, [], fn level, acc ->
        square = transform_input(input, level)

        Enum.with_index(square)
        |> Enum.map(fn {x, i} ->
          Enum.at(acc, i, []) ++ x
        end)
      end)
    end)
    |> part1()
  end
end
