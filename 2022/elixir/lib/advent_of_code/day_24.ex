defmodule AdventOfCode.Day24 do
  @moduledoc """
  Man, another pathfinding day??? Did an A* this time, took me way longer than it should've.
  Eventually I realized you can only wait in the current position if it is still empty next round.

  At least day 25 isn't another path-find, thank goodness.

  Here we go!
  """
  alias Prioqueue
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(fn line ->
      String.codepoints(line)
      |> Enum.map(fn
        "#" -> :wall
        "." -> :empty
        "<" -> [:left]
        ">" -> [:right]
        "^" -> [:up]
        "v" -> [:down]
      end)
    end)
    |> list_to_map()
  end

  def move({x, y}, :up), do: {x, y - 1}
  def move({x, y}, :down), do: {x, y + 1}
  def move({x, y}, :right), do: {x + 1, y}
  def move({x, y}, :left), do: {x - 1, y}

  @start {1, 0}
  # 4 * 6 grid
  @test_lcm 12
  # 25 * 120 grid
  @lcm 600

  def blizzards_by_minute(map, bounds, test \\ false) do
    # lcm of sides is the max number of minutes before the cycle repeats
    limit = if test, do: @test_lcm, else: @lcm

    Enum.reduce(0..limit, {%{}, map}, fn i, {result, i_acc} ->
      # put in a entry for the current minute, and pass result along for next minute
      {Map.put(result, i, i_acc), move_blizzards(i_acc, bounds)}
    end)
    |> elem(0)
  end

  def move_blizzard(map, {{x, y} = pos, type}, {x_max, y_max}) do
    next = {next_x, next_y} = move(pos, type)

    case Map.get(map, next) do
      :wall ->
        cond do
          next_x == 0 -> {x_max, y}
          next_x > x_max -> {1, y}
          next_y == 0 -> {x, y_max}
          next_y > y_max -> {x, 1}
        end

      _ ->
        next
    end
  end

  def move_blizzards(map, bounds) do
    Enum.reduce(map, %{}, fn {pos, type}, acc ->
      case type do
        :wall ->
          Map.put(acc, pos, :wall)

        :empty ->
          # don't overwrite new values with old empty
          new_val =
            case Map.get(acc, pos) do
              :empty -> :empty
              nil -> :empty
              x -> x
            end

          Map.put(acc, pos, new_val)

        _ ->
          Enum.map(type, fn t ->
            {move_blizzard(map, {pos, t}, bounds), t}
          end)
          |> Enum.reduce(acc, fn {p, t}, t_acc ->
            Map.update(t_acc, p, [t], fn val -> if(val == :empty, do: [t], else: [t | val]) end)
          end)
          |> Map.put_new(pos, :empty)
      end
    end)
  end

  def moves(map, pos) do
    get_adj(map, pos, all: false)
    # check all empty moves
    |> Stream.filter(fn {_p, t} -> t == :empty end)
    |> Enum.map(fn {p, _} -> p end)
    |> Enum.concat(if map[pos] == :empty, do: [pos], else: [])
  end

  def find_route(
        blizzards,
        queue,
        goal,
        test \\ false,
        seen \\ MapSet.new([{@start, 0}])
      ) do
    case Prioqueue.extract_min(queue) do
      {:ok, {{_f_cost, _h_cost, time, pos}, rest_queue}} ->
        if pos == goal do
          time
        else
          cycle = if test, do: @test_lcm, else: @lcm
          next_time = time + 1
          curr_map = blizzards[rem(next_time, cycle)]

          moves =
            moves(curr_map, pos)
            |> Enum.map(&{&1, next_time})
            |> Enum.reject(fn move -> MapSet.member?(seen, move) end)

          new_seen = Enum.reduce(moves, seen, &MapSet.put(&2, &1))

          new_queue =
            Enum.reduce(moves, rest_queue, fn {p, g_cost}, acc ->
              h_cost = manhattan_distance(p, goal)
              f_cost = h_cost + g_cost
              Prioqueue.insert(acc, {f_cost, h_cost, g_cost, p})
            end)

          find_route(
            blizzards,
            new_queue,
            goal,
            test,
            new_seen
          )
        end
    end
  end

  def part1(input, test \\ false) do
    {goal, _} = Enum.max_by(input, fn {{x, y}, t} -> if(t == :wall, do: 0, else: {y, x}) end)

    # state is tuple of f_cost (manhattan distance + time), h_cost (manhattan distance to goal), g_cost (time), and position
    queue =
      Prioqueue.new([
        {manhattan_distance(@start, goal), manhattan_distance(@start, goal), 0, @start}
      ])

    {x_bounds, y_bounds} = bounds(input)
    x_max = x_bounds[1] |> elem(1) |> Kernel.-(1)
    y_max = y_bounds[1] |> elem(1) |> Kernel.-(1)

    blizzards_by_minute(input, {x_max, y_max}, test)
    |> find_route(queue, goal, test)
  end

  def part2(input, test \\ false) do
    {goal, _} = Enum.max_by(input, fn {{x, y}, t} -> if(t == :wall, do: 0, else: {y, x}) end)

    start_queue = fn x ->
      Prioqueue.new([
        {manhattan_distance(@start, goal) + x, manhattan_distance(@start, goal), x, @start}
      ])
    end

    end_queue = fn x ->
      Prioqueue.new([
        {manhattan_distance(goal, @start) + x, manhattan_distance(goal, @start), x, goal}
      ])
    end

    {x_bounds, y_bounds} = bounds(input)
    x_max = x_bounds[1] |> elem(1) |> Kernel.-(1)
    y_max = y_bounds[1] |> elem(1) |> Kernel.-(1)

    blizzards = blizzards_by_minute(input, {x_max, y_max}, test)

    cycle = if test, do: @test_lcm, else: @lcm
    first = find_route(blizzards, start_queue.(0), goal, test)

    second =
      find_route(
        blizzards,
        end_queue.(first),
        @start,
        test,
        MapSet.new([{goal, rem(first, cycle)}])
      )

    find_route(
      blizzards,
      start_queue.(second),
      goal,
      test,
      MapSet.new([{goal, rem(second, cycle)}])
    )
  end
end
