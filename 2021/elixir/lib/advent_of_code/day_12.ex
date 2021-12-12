defmodule AdventOfCode.Day12 do
  alias AdventOfCode.Helpers.Graph

  def is_small_cave(cave), do: String.downcase(cave) == cave

  def find_paths(graph, vertex, duplicates \\ false, path \\ ["start"])

  def find_paths(_graph, _vertex, _duplicates, ["end" | _rest] = path),
    do: Enum.reverse(path) |> List.to_tuple()

  def find_paths(graph, vertex, duplicates, path) do
    possible_neighbors =
      Graph.neighbors(graph, vertex)
      |> Enum.filter(fn neighbor ->
        if is_small_cave(neighbor) do
          if duplicates and neighbor not in ["start", "end"] do
            # if no duplicates already, allow one small cave check
            Enum.filter(path, &is_small_cave/1)
            |> Enum.frequencies()
            |> Enum.all?(fn {_k, v} -> v < 2 end) or
              neighbor not in path
          else
            neighbor not in path
          end
        else
          true
        end
      end)

    Enum.map(possible_neighbors, fn neighbor ->
      find_paths(graph, neighbor, duplicates, [neighbor | path])
    end)
    |> List.flatten()
  end

  def part1(input) do
    Stream.map(input, &String.split(&1, "-"))
    |> Enum.reduce(Graph.new(), fn link, graph ->
      Graph.add_vertices(graph, link)
      |> Graph.add_edge(link)
    end)
    |> find_paths("start")
    |> Enum.count()
  end

  def part2(input) do
    Stream.map(input, &String.split(&1, "-"))
    |> Enum.reduce(Graph.new(), fn link, graph ->
      Graph.add_vertices(graph, link)
      |> Graph.add_edge(link)
    end)
    |> find_paths("start", true)
    |> Enum.count()
  end
end
