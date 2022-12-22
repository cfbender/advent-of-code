defmodule AdventOfCode.Day22 do
  @moduledoc """
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    [map, [directions]] =
      input
      |> divided_lines()

    map =
      Enum.map(map, fn line ->
        String.codepoints(line)
        |> Enum.map(fn
          " " -> nil
          "." -> :tile
          "#" -> :wall
        end)
      end)
      |> list_to_map()
      |> Enum.reject(fn {_, v} -> is_nil(v) end)
      |> Enum.into(%{})

    directions =
      String.split(directions, ~r/[LR]/, include_captures: true)
      |> Enum.map(fn dir ->
        case Integer.parse(dir) do
          {x, ""} -> x
          :error -> String.to_atom(dir)
        end
      end)

    {map, directions}
  end

  def turn(:R, :right), do: :down
  def turn(:R, :down), do: :left
  def turn(:R, :left), do: :up
  def turn(:R, :up), do: :right
  def turn(:L, :right), do: :up
  def turn(:L, :down), do: :right
  def turn(:L, :left), do: :down
  def turn(:L, :up), do: :left

  def get_x({{x, _}, _}), do: x
  def get_y({{_, y}, _}), do: y

  def move({x, y}, direction, {x_bounds, y_bounds}) do
    {x_min, x_max} = x_bounds[y]
    {y_min, y_max} = y_bounds[x]

    # couldn't really figure this out using rem/2
    # so sorry it's ugly
    case direction do
      :right ->
        if x == x_max do
          {x_min, y}
        else
          {x + 1, y}
        end

      :down ->
        if y == y_max do
          {x, y_min}
        else
          {x, y + 1}
        end

      :left ->
        if x == x_min do
          {x_max, y}
        else
          {x - 1, y}
        end

      :up ->
        if y == y_min do
          {x, y_max}
        else
          {x, y - 1}
        end
    end
  end

  def trace_path(_map, [], {location, facing}, _maxes), do: {location, facing}

  def trace_path(map, [direction | rest_dir], {location, facing}, maxes)
      when is_integer(direction) do
    new_location =
      Enum.reduce_while(1..direction, location, fn _, curr_location ->
        next = move(curr_location, facing, maxes)

        case Map.get(map, next) do
          :wall ->
            {:halt, curr_location}

          :tile ->
            {:cont, next}
        end
      end)

    trace_path(map, rest_dir, {new_location, facing}, maxes)
  end

  def trace_path(map, [direction | rest_dir], {location, facing}, maxes)
      when is_atom(direction) do
    new_facing = turn(direction, facing)
    trace_path(map, rest_dir, {location, new_facing}, maxes)
  end

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

  def password({{x, y}, facing}) do
    f_value =
      case facing do
        :right -> 0
        :down -> 1
        :left -> 2
        :up -> 3
      end

    (x + 1) * 4 + (y + 1) * 1000 + f_value
  end

  def part1(input) do
    {map, directions} = input

    {{leftmost, _}, _} =
      Enum.group_by(map, &get_y/1) |> Map.get(0) |> Enum.min_by(fn {x, _y} -> x end)

    start = {{leftmost, 0}, :right}
    maxes = bounds(map)

    trace_path(map, directions, start, maxes)
    |> password()
  end

  def part2(_args) do
  end
end
