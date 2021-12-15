defmodule AdventOfCode.Day15 do
  import AdventOfCode.Helpers

  alias AdventOfCode.Helpers.Graph

  @source {0, 0}

  def dijkstras(graph, edge_weights, dest) do
    # queue with {point, cost}
    dijkstras([{@source, 0}], graph, edge_weights, dest, MapSet.new([@source]))
  end

  def dijkstras(queue, graph, edge_weights, dest, visited) do
    [{node, cost} | rest_queue] = queue

    if node == dest do
      cost
    else
      neighbors =
        Graph.neighbors(graph, node)
        |> Enum.filter(fn point -> not MapSet.member?(visited, point) end)

      new_visited = Enum.concat(MapSet.to_list(visited), neighbors) |> MapSet.new()

      # add neighbor with added weight to queue
      Enum.map(neighbors, fn point -> {point, cost + Map.get(edge_weights, point)} end)
      |> Enum.concat(rest_queue)
      |> Enum.sort_by(fn {_point, cost} -> cost end, :asc)
      |> dijkstras(graph, edge_weights, dest, new_visited)
    end
  end

  def process_input(input),
    do: Enum.map(input, fn line -> String.codepoints(line) |> Enum.map(&String.to_integer/1) end)

  def part1(input) do
    edge_weights =
      input
      |> list_to_map()

    points = Map.keys(edge_weights)

    destination = Enum.max(points)

    Graph.new()
    |> Graph.add_vertices(points)
    |> then(fn start ->
      Enum.reduce(points, start, fn point, graph ->
        get_adj(edge_weights, point, all: false)
        |> Enum.reduce(graph, fn {adj, _val}, new_graph ->
          Graph.add_edge(new_graph, [point, adj])
        end)
      end)
    end)
    |> dijkstras(edge_weights, destination)
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
