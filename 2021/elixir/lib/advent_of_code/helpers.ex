defmodule AdventOfCode.Helpers do
  @doc """
  Converts a 2D list to a map of coordinates with the given value

  ## Examples

      iex> Helpers.list_to_map([[1,2,3], [4,5,6]])
      %{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}
  """
  def list_to_map(list) do
    Enum.with_index(list)
    |> Enum.reduce(Map.new(), fn {row, y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {val, x}, row_acc ->
        Map.put(row_acc, {x, y}, val)
      end)
    end)
  end

  @doc """
  Gets all adjacent points on a map of XY coordinates to a point

  Pass `all: false` to not include diagonally adjacent points

  ## Examples
      iex> Helpers.get_adj(%{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}, {1,1})
      %{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}
  """
  def get_adj(map, {x, y}, [all: all] \\ [all: true]) do
    deltas =
      if all,
        do: for(x <- [-1, 0, 1], y <- [-1, 0, 1], do: {x, y}),
        else: [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]

    deltas
    |> Enum.reduce(Map.new(), fn {dx, dy}, acc ->
      new_point = {x + dx, y + dy}

      new_point_val = Map.get(map, new_point)

      if new_point_val do
        Map.put(acc, new_point, new_point_val)
      else
        acc
      end
    end)
  end

  defmodule Graph do
    defstruct vertices: MapSet.new(), edges: MapSet.new()

    def new, do: %Graph{}

    def add_vertex(%Graph{vertices: vertices} = graph, vertex) do
      Map.put(graph, :vertices, MapSet.put(vertices, vertex))
    end

    def add_vertices(graph, vertices) do
      Enum.reduce(vertices, graph, fn vertex, acc ->
        Map.put(acc, :vertices, MapSet.put(acc.vertices, vertex))
      end)
    end

    def add_edge(%{edges: edges} = graph, edge) do
      Map.put(graph, :edges, MapSet.put(edges, MapSet.new(edge)))
    end

    def neighbors(%{edges: edges}, vertex) do
      MapSet.to_list(edges)
      |> Stream.filter(fn edge -> MapSet.member?(edge, vertex) end)
      |> Stream.flat_map(&MapSet.to_list/1)
      |> Stream.dedup()
      |> Enum.filter(&(&1 != vertex))
    end

    defimpl Inspect, for: Graph do
      import Inspect.Algebra

      def inspect(graph, opts) do
        concat([
          "%Graph{\n     vertices: ",
          to_doc(MapSet.to_list(graph.vertices), opts),
          ",\n     edges: ",
          to_doc(MapSet.to_list(graph.edges) |> Enum.map(&MapSet.to_list/1), opts)
        ])
      end
    end
  end
end
