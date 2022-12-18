defmodule AdventOfCode.Day17 do
  @moduledoc """
  I gave the fuck up on this one. I knew exactly what I needed to do with the cycles, figured that part out myself.

  Ended up basically just stealing https://github.com/mathsaey/adventofcode/blob/master/lib/2022/17.ex
  and refactoring it a bit to match mine more closely. It's much better than what I could've done anyway. Check the previous
  commit on this file for my part 1.
  """
  def parse_input(input) do
    input
    |> String.trim()
    |> String.codepoints()
    |> Enum.map(fn a -> if(a == "<", do: :left, else: :right) end)
  end

  @rock_order [:-, :+, :j, :|, :o]
  @rocks %{
    # ####
    -: [{2, 0}, {3, 0}, {4, 0}, {5, 0}],
    # .#.
    # ###
    # .#.
    +: [{3, 2}, {2, 1}, {3, 1}, {4, 1}, {3, 0}],
    # ..#
    # ..#
    # ###
    j: [{4, 2}, {4, 1}, {2, 0}, {3, 0}, {4, 0}],
    #
    #
    #
    #
    |: [{2, 3}, {2, 2}, {2, 1}, {2, 0}],
    ##
    ##
    o: [{2, 1}, {3, 1}, {2, 0}, {3, 0}]
  }

  def part1(input), do: input |> run_n_blocks(2022) |> height()
  def part2(input), do: input |> find_height_with_cycles(1_000_000_000_000)

  def initial_state(input) do
    %{
      heights: 0 |> List.duplicate(7) |> List.to_tuple(),
      field: MapSet.new(),
      next_moves: input,
      moves: input,
      prev_states: %{},
      prev: %{}
    }
  end

  def run_n_blocks(input, n) do
    Stream.cycle(@rock_order)
    |> Stream.scan(initial_state(input), &simulate/2)
    |> Stream.drop(n - 1)
    |> Enum.take(1)
    |> hd()
  end

  def find_height_with_cycles(input, n) do
    {idx, state, prev_state} =
      Stream.cycle(@rock_order)
      |> Stream.with_index(1)
      |> Enum.reduce_while(initial_state(input), fn {rock, idx}, state ->
        top = height(state)
        offsets = state.heights |> Tuple.to_list() |> Enum.map(&(top - &1))
        key = {rock, state[:next_moves], offsets}

        case state.prev_states[key] do
          nil ->
            state = put_in(state, [:prev_states, key], Map.put(state, :idx, idx))
            {:cont, simulate(rock, state)}

          prev_state ->
            {:halt, {idx, state, prev_state}}
        end
      end)

    cycle_length = idx - prev_state[:idx]
    remaining_steps = n - idx

    cycle_times = div(remaining_steps, cycle_length)
    offset = rem(remaining_steps, cycle_length)

    height_increase = height(state) - height(prev_state)
    state = run_n_blocks(input, idx + offset)
    height(state) + height_increase * cycle_times
  end

  def simulate(rock, state) when is_atom(rock),
    do: drop_rock(rock, state) |> simulate(state)

  def simulate(falling, state = %{next_moves: [], moves: m}),
    do: simulate(falling, %{state | next_moves: m})

  def simulate(falling, state = %{next_moves: [dir | next]}) do
    move(falling, dir, state)
    |> fall(state)
    |> case do
      {:stuck, state} -> %{state | next_moves: next}
      {:continue, falling} -> simulate(falling, %{state | next_moves: next})
    end
  end

  def fall(falling, state = %{field: field, heights: heights}) do
    next = Enum.map(falling, fn {x, y} -> {x, y - 1} end)

    if invalid?(next, state) do
      field = Enum.reduce(falling, field, &MapSet.put(&2, &1))

      heights =
        Enum.reduce(falling, heights, fn {x, y}, acc ->
          put_elem(acc, x, max(y + 1, elem(acc, x)))
        end)

      {:stuck, %{state | field: field, heights: heights}}
    else
      {:continue, next}
    end
  end

  def move(block, :left, state), do: push(block, fn {x, y} -> {x - 1, y} end, state)
  def move(block, :right, state), do: push(block, fn {x, y} -> {x + 1, y} end, state)

  def push(block, fun, state) do
    next = Enum.map(block, fun)
    if(invalid?(next, state), do: block, else: next)
  end

  def drop_rock(rock, state) do
    top = height(state)
    Map.get(@rocks, rock) |> Enum.map(fn {x, y} -> {x, top + 3 + y} end)
  end

  def invalid?(b, %{field: field}) do
    Enum.any?(b, fn t = {x, y} -> y < 0 || x not in 0..6 || t in field end)
  end

  def height(%{heights: h}), do: h |> Tuple.to_list() |> Enum.max()
end
