defmodule AdventOfCode.Day22 do
  @moduledoc """
  What a fuckin part 2. Part 1 was pretty easy and fun, though the wrap logic was kinda annoying to get right,
  but I just solved it with using the bounds of each slice. Part 2 on the other hand was a monster.

  Ended up just hard coding the side relationships after modeling it in Fusion360, though it should work for
  all inputs, since Topaz said they are all the same shape.

  Fun, but too long. Hopefully that's the last long problem this year as we get closer to Christmas.
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

  def flat_move({x, y} = pos, direction, {x_bounds, y_bounds}) do
    {x_min, x_max} = x_bounds[y]
    {y_min, y_max} = y_bounds[x]

    # couldn't really figure this out using rem/2
    # so sorry it's ugly
    {case direction do
       :right ->
         if x == x_max do
           {x_min, y}
         else
           move(pos, direction)
         end

       :down ->
         if y == y_max do
           {x, y_min}
         else
           move(pos, direction)
         end

       :left ->
         if x == x_min do
           {x_max, y}
         else
           move(pos, direction)
         end

       :up ->
         if y == y_min do
           {x, y_max}
         else
           move(pos, direction)
         end
     end, direction}
  end

  def move({x, y}, :right), do: {x + 1, y}
  def move({x, y}, :down), do: {x, y + 1}
  def move({x, y}, :left), do: {x - 1, y}
  def move({x, y}, :up), do: {x, y - 1}

  def trace_path(map, directions, position, maxes, opts \\ [])
  def trace_path(_map, [], {location, facing}, _maxes, _opts), do: {location, facing}

  def trace_path(map, [direction | rest_dir], position, maxes, opts)
      when is_integer(direction) do
    test = opts[:test] || false
    cube = opts[:cube] || false

    {new_location, new_facing} =
      Enum.reduce_while(1..direction, position, fn _, {curr_location, curr_facing} ->
        {next, next_facing} =
          if(cube,
            do: cube_move(map, curr_location, curr_facing, test),
            else: flat_move(curr_location, curr_facing, maxes)
          )

        case Map.get(map, next) do
          :wall ->
            {:halt, {curr_location, curr_facing}}

          :tile ->
            {:cont, {next, next_facing}}
        end
      end)

    trace_path(map, rest_dir, {new_location, new_facing}, maxes, opts)
  end

  def trace_path(map, [direction | rest_dir], {location, facing}, maxes, opts)
      when is_atom(direction) do
    new_facing = turn(direction, facing)
    trace_path(map, rest_dir, {location, new_facing}, maxes, opts)
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

  def part1(input, test \\ false) do
    {map, directions} = input

    {{leftmost, _}, _} =
      Enum.group_by(map, &get_y/1) |> Map.get(0) |> Enum.min_by(fn {x, _y} -> x end)

    start = {{leftmost, 0}, :right}
    maxes = bounds(map)

    trace_path(map, directions, start, maxes, test: test)
    |> password()
  end

  def find_square({x, y}, test) do
    if test do
      cond do
        x in 8..11 and y in 0..3 -> 1
        x in 0..3 and y in 4..7 -> 2
        x in 4..7 and y in 4..7 -> 3
        x in 8..11 and y in 4..7 -> 4
        x in 8..11 and y in 8..11 -> 5
        x in 12..15 and y in 8..11 -> 6
      end
    else
      cond do
        x in 50..99 and y in 0..49 -> 1
        x in 100..149 and y in 0..49 -> 2
        x in 50..99 and y in 50..99 -> 3
        x in 0..49 and y in 100..149 -> 4
        x in 50..99 and y in 100..149 -> 5
        x in 0..49 and y in 150..199 -> 6
      end
    end
  end

  def cube_move(map, {x, y} = position, direction, test) do
    next = move(position, direction)
    valid = Map.has_key?(map, next)

    if valid do
      {next, direction}
    else
      if test do
        case {find_square(position, test), direction} do
          # to square 2 - top left of 1 goes to top right of 2
          {1, :up} -> {{11 - x, 4}, :down}
          # to square 3 - top left of 1 goes to top left of 3
          {1, :left} -> {{4 + y, 4}, :down}
          # to square 6 - top right of 1 goes to bottom right of 6
          {1, :right} -> {{x + 4, 11 - y}, :left}
          # to square 6 - top left of two goes to bottom right of 6
          {2, :left} -> {{11 - y + 4, 11}, :up}
          # to square 1 - top left of two goes to top right of 1
          {2, :up} -> {{11 - x, 0}, :down}
          # to square 5 - bottom left of two goes to bottom right of 5
          {2, :down} -> {{11 - x, 11}, :up}
          # to square 1 - top left of 3 goes to top left of 1
          {3, :up} -> {{8, x - 4}, :right}
          # to square 5 - bottom left of 3 goes to bottom left of 5
          {3, :down} -> {{8, 11 - x + 4}, :right}
          # to square 6 - top right of 4 goes to top right of 6
          {4, :right} -> {{15 - y + 4, 8}, :down}
          # to square 3 - top left of 5 goes to bottom right of 3
          {5, :left} -> {{8 - y + 7, 7}, :up}
          # to square 2 - bottom left of 5 goes to bottom right of 2
          {5, :down} -> {{11 - x, 7}, :up}
          # to square 4 - top left of 6 goes to bottom right of 4
          {6, :up} -> {{11, 15 - x + 4}, :left}
          # to square 1 - top right of 6 goes to bottom right of 1
          {6, :right} -> {{11, 11 - y}, :left}
          # to square 2 - bottom left of 6 goes to bottom left of 2
          {6, :down} -> {{0, 15 - x + 4}, :right}
        end
      else
        # numbered sides:
        #   1 2
        #   3
        # 4 5
        # 6
        case {find_square(position, test), direction} do
          # to square 6 - top left of 1 goes to top left of 6
          {1, :up} -> {{0, 100 + x}, :right}
          # to square 4 - top left of 1 goes to bottom left of 4
          {1, :left} -> {{0, 149 - y}, :right}
          # to square 5 - top right of 2 goes to bottom right of 5
          {2, :right} -> {{99, 149 - y}, :left}
          # to square 6 - top left of two goes to bottom left of 6
          {2, :up} -> {{x - 100, 199}, :up}
          # to square 3 - bottom left of 2 goes to top right of 3
          {2, :down} -> {{99, x - 50}, :left}
          # to square 4 - top left of 3 goes to top left of 4
          {3, :left} -> {{y - 50, 100}, :down}
          # to square 2  - top right of 3 goes to bottom right of 2
          {3, :right} -> {{y + 50, 49}, :up}
          # to square 3 - top left of 4 goes to top left of 3
          {4, :up} -> {{50, x + 50}, :right}
          # to square 1 - top left of 4 goes to bottom left of 1
          {4, :left} -> {{50, 149 - y}, :right}
          # to square 2 - top right of 5 goes to bottom right of 2
          {5, :right} -> {{149, 149 - y}, :left}
          # to square 6 - bottom left of 5 goes to top right of 6
          {5, :down} -> {{49, 100 + x}, :left}
          # to square 1 - top left of 6 goes to top left of 1
          {6, :left} -> {{y - 100, 0}, :down}
          # to square 5 - top right of 6 goes to bottom left of 5
          {6, :right} -> {{y - 100, 149}, :up}
          # to square 2 - bottom left of 6 goes to top left of 2
          {6, :down} -> {{x + 100, 0}, :down}
        end
      end
    end
  end

  def part2(input, test \\ false) do
    {map, directions} = input

    {{leftmost, _}, _} =
      Enum.group_by(map, &get_y/1) |> Map.get(0) |> Enum.min_by(fn {x, _y} -> x end)

    start = {{leftmost, 0}, :right}

    trace_path(map, directions, start, nil, cube: true, test: test)
    |> password()
  end
end
