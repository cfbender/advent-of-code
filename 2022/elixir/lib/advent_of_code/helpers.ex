defmodule AdventOfCode.Helpers do
  @doc """
  Converts a 2D list to a map of coordinates with the given value

  ## Examples

      iex> Helpers.list_to_map([[1,2,3], [4,5,6]])
      %{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}
  """
  @spec list_to_map([[any()]]) :: map()
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
  Prints out a visualization of a provided map of {x,y} points.
  Defaults to y=0 on the top, can be switched with the `inverted` option as `true.`
  Also accepts a display function that will pass the value of the key at that point to format the value.
  ## Examples

      iex> Helpers.list_to_map([[true,false,true], [false,true,false]]) |> Helpers.print_map()

      #.#
      .#.
      %{
      {0, 0} => true,
      {0, 1} => false,
      {1, 0} => false,
      {1, 1} => true,
      {2, 0} => true,
      {2, 1} => false
      }

      iex> Helpers.list_to_map([[true,false,true], [false,true,false]]) |> Helpers.print_map([inverted: true, display: fn x -> if x, do: "😂", else: "⬛" end])

      ⬛😂⬛
      😂⬛😂
      %{
        {0, 0} => true,
        {0, 1} => false,
        {1, 0} => false,
        {1, 1} => true,
        {2, 0} => true,
        {2, 1} => false
      } 
  """
  @spec print_map(map()) :: map()
  def print_map(map, opts \\ []) do
    inverted = opts[:inverted] || false
    display = opts[:display] || fn x -> if x, do: "#", else: "." end

    {{{x_min, _}, _}, {{x_max, _}, _}} = Enum.min_max_by(map, fn {{x, _}, _} -> x end)
    {{{_, y_min}, _}, {{_, y_max}, _}} = Enum.min_max_by(map, fn {{_, y}, _} -> y end)
    y_range = if inverted, do: y_max..y_min, else: y_min..y_max

    IO.write("\n")

    Enum.each(y_range, fn y ->
      Enum.each(x_min..x_max, fn x ->
        Map.get(map, {x, y})
        |> display.()
        |> IO.write()
      end)

      IO.write("\n")
    end)

    map
  end

  def enqueue(queue, []), do: queue

  def enqueue(queue, [i | tail]) do
    :queue.in(i, queue)
    |> enqueue(tail)
  end

  def enqueue(queue, val), do: enqueue(queue, [val])

  def dequeue(queue), do: :queue.out(queue)

  def lines(string), do: String.split(string, "\n", trim: true)

  def divided_lines(string),
    do: String.split(string, "\n\n", trim: true) |> Enum.map(&String.split(&1, "\n", trim: true))

  @doc """
  Gets the Euclidean distance between two points

  ## Examples

      iex> Helpers.euclidean_distance({-2, 4}, {3, 3})
      5.0990195135927845
  """

  @spec euclidean_distance({number, number}, {number, number}) :: float
  def euclidean_distance({x1, y1}, {x2, y2}) do
    Integer.pow(abs(x2 - x1), 2)
    |> Kernel.+(Integer.pow(abs(y2 - y1), 2))
    |> :math.sqrt()
  end

  @doc """
  Gets all adjacent points on a map of XY coordinates to a point

  Pass `all: false` to not include diagonally adjacent points
  Pass `default: <value>` to retrieve some other value than nil by default
  Pass `three_d: true` to use a three dimensional map

  ## Examples
      iex> Helpers.get_adj(%{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}, {1,1})
      %{{0, 0} => 1, {0, 1} => 4, {1, 0} => 2, {1, 1} => 5, {2, 0} => 3, {2, 1} => 6}
  """
  def get_adj(map, {x, y}, opts \\ []) do
    default = Keyword.get(opts, :default, nil)
    all = Keyword.get(opts, :all, true)
    three_d = Keyword.get(opts, :three_d, false)

    deltas =
      cond do
        all ->
          for(x <- [-1, 0, 1], y <- [-1, 0, 1], do: {x, y})

        three_d ->
          for(x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1], do: {x, y, z})

        true ->
          [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
      end

    deltas
    |> Enum.reduce(Map.new(), fn {dx, dy}, acc ->
      new_point = {x + dx, y + dy}

      new_point_val = Map.get(map, new_point, default)

      if new_point_val do
        Map.put(acc, new_point, new_point_val)
      else
        acc
      end
    end)
  end

  @doc """
  Gets shortest path to a destination.

  """
  @spec dijkstras(
          map(),
          {number(), number()},
          {number(), number()},
          (number(), any(), any() -> number() | :infinity)
        ) :: number()
  def dijkstras(map, start, dest, get_cost) do
    dijkstras(
      [{start, 0}],
      map,
      dest,
      Map.new(Enum.map(Map.keys(map), &{&1, :infinity})),
      get_cost
    )
  end

  def dijkstras([], _map, _dest, _costs, _get_cost), do: :infinity

  def dijkstras(queue, map, dest, costs, get_cost) do
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
          new_cost = get_cost.(cost, start, next)

          if new_cost < current_cost do
            {[{neighbor, new_cost} | q_acc], Map.put(c_acc, neighbor, new_cost)}
          else
            acc
          end
        end)

      dijkstras(Enum.sort(new_queue), map, dest, new_costs, get_cost)
    end
  end

  defmodule Graph do
    @moduledoc """
     A simple graph data structure with edges and vertices
    """
    @type t :: %__MODULE__{vertices: MapSet.t(any), edges: MapSet.t(MapSet.t(any))}
    defstruct vertices: MapSet.new(), edges: MapSet.new(), neighbors: %{}

    @spec new :: %Graph{}
    def new, do: %Graph{}

    @doc """
    Add vertex to graph. Is idempotent.

    """
    @spec add_vertex(%Graph{}, any) :: %Graph{}
    def add_vertex(%Graph{vertices: vertices} = graph, vertex) do
      Map.put(graph, :vertices, MapSet.put(vertices, vertex))
    end

    @doc """
    Add vertices in a lit to graph. Is idempotent for individual vertices.

    """
    @spec add_vertices(%Graph{}, list(any)) :: %Graph{}
    def add_vertices(graph, vertices) do
      Enum.reduce(vertices, graph, fn vertex, acc ->
        Map.put(acc, :vertices, MapSet.put(acc.vertices, vertex))
      end)
    end

    @doc """
    Add edge to graph. Is idempotent.

    """
    @spec add_edge(%Graph{}, any) :: %Graph{}
    def add_edge(%Graph{edges: edges} = graph, edge) do
      Map.put(graph, :edges, MapSet.put(edges, MapSet.new(edge)))
      |> Map.update!(:neighbors, fn neighbors ->
        [edge_a, edge_b] = edge

        Map.update(neighbors, edge_a, MapSet.new(), &MapSet.put(&1, edge_b))
        |> Map.update(edge_b, MapSet.new(), &MapSet.put(&1, edge_a))
      end)
    end

    @doc """
    Get all vertices connected to a given vertex.

    """
    @spec neighbors(%Graph{}, any) :: list
    def neighbors(%Graph{neighbors: neighbors}, vertex) do
      Map.get(neighbors, vertex) |> MapSet.to_list()
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
