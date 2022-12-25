defmodule AdventOfCode.Day24 do
  @moduledoc """
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

  # def compare_states({p_a, t_a}, {p_b, t_b}) do
  #   cond do
  #     p_a < p_b -> :gt
  #     p_a > p_b -> :lt
  #     t_a < t_b -> :lt
  #     t_a > t_b -> :gt
  #     t_a == t_b -> :eq
  #   end
  # end

  def moves(map, pos, time, seen) do
    get_adj(map, pos, all: false)
    # check all empty moves
    |> Stream.filter(fn {p, t} -> t == :empty end)
    |> Enum.map(fn {p, _} -> {p, time + 1} end)
    |> IO.inspect()
    |> then(fn moves ->
      if length(moves) == 0 do
        [{pos, time + 1}]
      else
        moves
      end
    end)
  end

  def find_route(
        blizzards,
        queue,
        goal,
        test \\ false,
        best \\ :infinity,
        seen \\ MapSet.new([{@start, 0}])
      ) do
    case Prioqueue.extract_min(queue) do
      {:error, :empty} ->
        best

      {:ok, {{pos, time}, rest_queue}} ->
        cycle = if test, do: @test_lcm, else: @lcm
        curr_map = blizzards[rem(time + 1, cycle)]
        moves = moves(curr_map, pos, time, seen)

        if goal in Enum.map(moves, &elem(&1, 0)) do
          new_best = min(time + 1, best)

          find_route(
            blizzards,
            rest_queue,
            goal,
            test,
            new_best,
            seen
          )
        else
          new_seen = MapSet.put(seen, {pos, time})

          new_queue = Enum.reduce(moves, rest_queue, &Prioqueue.insert(&2, &1))

          find_route(
            blizzards,
            new_queue,
            goal,
            test,
            best,
            new_seen
          )
        end
    end
  end

  def print(map) do
    print_map(map,
      display: fn
        x ->
          case x do
            :empty ->
              "."

            :wall ->
              "#"

            list ->
              if length(list) == 1 do
                case Enum.at(list, 0) do
                  :right -> ">"
                  :left -> "<"
                  :up -> "^"
                  :down -> "v"
                end
              else
                "#{length(list)}"
              end
          end
      end
    )

    map
  end

  def part1(input, test \\ false) do
    # 218 too low
    {goal, _} = Enum.max_by(input, fn {{x, y}, t} -> if(t == :wall, do: 0, else: {y, x}) end)

    # state is tuple of position and number of minutes
    queue = Prioqueue.new([{@start, 0}])
    {x_bounds, y_bounds} = bounds(input)
    x_max = x_bounds[1] |> elem(1) |> Kernel.-(1)
    y_max = y_bounds[1] |> elem(1) |> Kernel.-(1)

    blizzards_by_minute(input, {x_max, y_max}, test)
    |> find_route(queue, goal, test)

    # |> Kernel.+(1)
  end

  def part2(_args) do
  end
end
