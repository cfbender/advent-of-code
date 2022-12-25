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

  @doc """
  Extracts just the x value from a coordinate

  ## Examples

      iex> Helpers.get_x({{2, 3}, :pizza})
      2
  """
  @spec get_x({{number(), number()}, any()}) :: number()
  def get_x({{x, _}, _}), do: x

  @doc """
  Extracts just the y value from a coordinate

  ## Examples

      iex> Helpers.get_y({{2, 3}, :pizza})
      3
  """
  @spec get_y({{number(), number()}, any()}) :: number()
  def get_y({{_, y}, _}), do: y

  @doc """
  Slices a 2D map and finds the x bounds for each y, and the y bounds for each x

  The bounds are a tuple of maps, where each contains the bounds for the given type.
  The map expects the opposite type of coordinate as a lookup value.

  Eg. a grid with shape:
      1 2 3
      4 5

    will have x bounds of 0..2 for y=0 and 0..1 for y=1. 
    For x=0 and x=1, the y bounds will be 0..1, but x=2 will be 0..0
  ## Examples

      iex> Helpers.list_to_map([[1,2,3], [4,5]]) |> Helpers.bounds()
      {%{0 => {0, 2}, 1 => {0, 1}}, %{0 => {0, 1}, 1 => {0, 1}, 2 => {0, 0}}}
  """

  @spec bounds(map()) :: {map(), map()}
  def bounds(map) do
    x_bounds =
      Enum.group_by(map, &get_y/1)
      |> Enum.map(fn {y, vals} ->
        {{{x_min, _}, _}, {{x_max, _}, _}} = Enum.min_max_by(vals, &get_x/1)
        {y, {x_min, x_max}}
      end)
      |> Enum.into(%{})

    y_bounds =
      Enum.group_by(map, &get_x/1)
      |> Enum.map(fn {x, vals} ->
        {{{_, y_min}, _}, {{_, y_max}, _}} = Enum.min_max_by(vals, &get_y/1)
        {x, {y_min, y_max}}
      end)
      |> Enum.into(%{})

    {x_bounds, y_bounds}
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
  Gets the Manhattan distance between two points

  ## Examples

      iex> Helpers.manhattan_distance({-2, 4}, {3, 3})
      6
  """
  @spec manhattan_distance({number, number}, {number, number}) :: number()
  def manhattan_distance({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  @doc """
  Reflects a point across the y-axis for a given origin

  ## Examples
      iex> Helpers.reflect_x({0, 0}, {-2, 3})
      {2, 3}

      iex> Helpers.reflect_x({-2, 4}, {3, 3})
      {-7, 3}
  """
  @spec reflect_x({number, number}, {number, number}) :: {number(), number()}
  def reflect_x(point, origin) do
    {ox, _oy} = origin
    {px, py} = point
    distance = ox - px
    {ox + distance, py}
  end

  @doc """
  Reflects a point across the x-axis for a given origin

  ## Examples
      iex> Helpers.reflect_y({0, 0}, {-2, 3})
      {-2, -3}

      iex> Helpers.reflect_y({-2, 4}, {3, 3})
      {3, 5}
  """
  @spec reflect_y({number, number}, {number, number}) :: {number(), number()}
  def reflect_y(point, origin) do
    {_ox, oy} = origin
    {px, py} = point
    distance = oy - py
    {px, oy + distance}
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
  def get_adj(map, point, opts \\ []) do
    default = Keyword.get(opts, :default, nil)
    all = Keyword.get(opts, :all, true)
    three_d = Keyword.get(opts, :three_d, false)

    deltas =
      cond do
        all ->
          for(x <- [-1, 0, 1], y <- [-1, 0, 1], do: {x, y})

        three_d and all ->
          for(x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1], do: {x, y, z})

        three_d ->
          [{-1, 0, 0}, {0, -1, 0}, {0, 0, -1}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}]

        true ->
          [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
      end

    {x, y, z} =
      case point do
        {x, y} -> {x, y, nil}
        {x, y, z} -> {x, y, z}
      end

    Enum.reject(deltas, fn d -> Tuple.to_list(d) |> Enum.all?(&(&1 == 0)) end)
    |> Enum.reduce(Map.new(), fn delta, acc ->
      new_point =
        case delta do
          {dx, dy} ->
            {x + dx, y + dy}

          {dx, dy, dz} ->
            {x + dx, y + dy, z + dz}
        end

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

    @doc """
    Create new graph with a list of vertices and a list of edges

    """
    def new, do: %Graph{}

    @spec new([any()], [any()]) :: %Graph{}
    def new(vertices, edges) do
      add_vertices(new(), vertices)
      |> add_edges(edges)
    end

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
    @spec add_edge(%Graph{}, [any]) :: %Graph{}
    def add_edge(%Graph{edges: edges} = graph, edge) do
      Map.put(graph, :edges, MapSet.put(edges, MapSet.new(edge)))
      |> Map.update!(:neighbors, fn neighbors ->
        [edge_a, edge_b] = edge

        Map.update(neighbors, edge_a, MapSet.new(), &MapSet.put(&1, edge_b))
        |> Map.update(edge_b, MapSet.new(), &MapSet.put(&1, edge_a))
      end)
    end

    @doc """
    Add list of edges to graph. Is idempotent.

    """
    @spec add_edges(%Graph{}, [[any]]) :: %Graph{}
    def add_edges(%Graph{edges: _edges} = graph, new_edges) do
      Enum.reduce(new_edges, graph, &add_edge(&2, &1))
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
          to_doc(MapSet.to_list(graph.edges) |> Enum.map(&MapSet.to_list/1), opts),
          "\n}"
        ])
      end
    end

    @doc """
    Calculates shortest paths between all pairs of vertices
    https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
    """
    def floyd_warshall(graph, backtracking \\ true, max \\ :infinity) do
      cap_add = fn a, b ->
        if is_atom(max) do
          if a == max or b == max do
            max
          else
            a + b
          end
        else
          a + b
        end
      end

      initial =
        for i <- graph.vertices,
            j <- graph.vertices,
            into: %{},
            do: {{i, j}, if(i == j, do: 0, else: max)}

      Enum.reduce(graph.edges, initial, fn edge, acc ->
        [a, b] = MapSet.to_list(edge)

        map = Map.put(acc, {a, b}, 1)

        if backtracking do
          Map.put(map, {b, a}, 1)
        else
          map
        end
      end)
      |> then(fn dist ->
        for(k <- graph.vertices, i <- graph.vertices, j <- graph.vertices, do: {k, i, j})
        |> Enum.reduce(dist, fn {k, i, j}, dst ->
          if dst[{i, j}] > cap_add.(dst[{i, k}], dst[{k, j}]) do
            Map.put(dst, {i, j}, cap_add.(dst[{i, k}], dst[{k, j}]))
          else
            dst
          end
        end)
      end)
    end
  end
end
