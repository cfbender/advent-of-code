defmodule AdventOfCode.Day25 do
  import AdventOfCode.Helpers

  def map_input(input) do
    Enum.map(input, &String.codepoints/1)
    |> Enum.map(fn line ->
      Enum.map(line, fn char ->
        case char do
          ">" -> :east
          "v" -> :south
          _ -> nil
        end
      end)
    end)
    |> list_to_map()
  end

  def get_neighbor({{x, y}, type}, {map, {max_x, max_y}}) do
    end_range = if type == :east, do: max_x, else: max_y
    cycle = Stream.cycle(0..end_range)
    [next_x, next_y] = Enum.map([x, y], fn coord -> Enum.at(cycle, coord + 1) end)

    next_pos =
      if type == :east, do: {next_x, y}, else: if(type == :south, do: {x, next_y}, else: nil)

    {next_pos, Map.get(map, next_pos)}
  end

  # in goal state when all cucs have neighbor
  def submarine_space?({map, _bounds} = state) do
    Enum.filter(map, fn {_, type} -> not is_nil(type) end)
    |> Enum.all?(fn cuc ->
      get_neighbor(cuc, state) |> elem(1)
    end)
  end

  def move(cuc = {coord, type}, new_map, state) do
    # use outer map to do all simultaneously
    {neighbor_pos, neighbor} = get_neighbor(cuc, state)

    unless neighbor do
      Map.put(new_map, coord, nil)
      |> Map.put(neighbor_pos, type)
    else
      new_map
    end
  end

  def step({map, bounds} = state, count \\ 1) do
    if submarine_space?(state) do
      count
    else
      east_herd = Enum.filter(map, fn {_, type} -> type == :east end)
      south_herd = Enum.filter(map, fn {_, type} -> type == :south end)

      new_map =
        Enum.reduce(east_herd, map, &move(&1, &2, state))
        |> then(fn map -> Enum.reduce(south_herd, map, &move(&1, &2, {map, bounds})) end)

      step({new_map, bounds}, count + 1)
    end
  end

  def part1(input) do
    map = map_input(input)
    {{max_x, max_y}, _} = Enum.max(map) |> IO.inspect()

    step({map, {max_x, max_y}})
  end

  def part2(_args) do
  end
end
