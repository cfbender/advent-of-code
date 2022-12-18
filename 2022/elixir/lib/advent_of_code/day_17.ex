defmodule AdventOfCode.Day17 do
  @moduledoc """
  """
  import AdventOfCode.Helpers

  require Record

  def parse_input(input) do
    input
    |> String.trim()
    |> String.codepoints()
    |> Enum.map(fn a -> if(a == "<", do: :left, else: :right) end)
  end

  # points with distance from left wall, and distance above bottom
  @rocks [
    # ####
    [{2, 0}, {3, 0}, {4, 0}, {5, 0}],
    # .#.
    # ###
    # .#.
    [{3, 2}, {2, 1}, {3, 1}, {4, 1}, {3, 0}],
    # ..#
    # ..#
    # ###
    [{4, 2}, {4, 1}, {2, 0}, {3, 0}, {4, 0}],
    #
    #
    #
    #
    [{2, 3}, {2, 2}, {2, 1}, {2, 0}],
    ##
    ##
    [{2, 1}, {3, 1}, {2, 0}, {3, 0}]
  ]

  @x_min 0
  @x_max 6

  @floor for(x <- @x_min..@x_max, do: {x, 0}) |> MapSet.new()

  Record.defrecord(:state,
    falling: nil,
    points: @floor,
    highest: 0,
    jet_count: 0,
    rocks_count: 0,
    removed: 0
  )

  def drop_rock(state(rocks_count: rocks_count, highest: highest) = curr_state, rocks) do
    # Each rock appears so that its bottom edge is
    # three units above the highest rock in the room (or the floor, if there isn't one).
    rock = Enum.at(rocks, rocks_count)
    new_rock = Enum.map(rock, fn {x, y} -> {x, y + highest + 4} end)
    state(curr_state, falling: new_rock, rocks_count: rocks_count + 1)
  end

  def push(state(falling: rock, points: points, jet_count: jet_count) = curr_state, jets) do
    direction = Enum.at(jets, jet_count)

    should_push =
      case direction do
        :right ->
          not Enum.any?(rock, fn {x, y} ->
            x + 1 > @x_max or MapSet.member?(points, {x + 1, y})
          end)

        :left ->
          not Enum.any?(rock, fn {x, y} ->
            x - 1 < @x_min or MapSet.member?(points, {x - 1, y})
          end)
      end

    dx =
      cond do
        should_push and direction == :right -> 1
        should_push and direction == :left -> -1
        true -> 0
      end

    new_rock =
      Enum.map(rock, fn {x, y} ->
        {x + dx, y}
      end)

    state(curr_state, falling: new_rock, jet_count: jet_count + 1)
  end

  def collision?(state(falling: falling, points: points)) do
    falling &&
      Enum.any?(falling, fn {x, y} -> MapSet.member?(points, {x, y}) end)
  end

  def fall(
        state(points: points, highest: highest) = curr_state,
        jets
      ) do
    new_state = push(curr_state, jets)
    state(falling: falling) = new_state
    new_rock = Enum.map(falling, fn {x, y} -> {x, y - 1} end)

    new_state = state(new_state, falling: new_rock)

    next_state =
      if collision?(new_state) do
        # use pushed rock since push happened before collision
        {_, y_max} = Enum.max_by(falling, fn {_, y} -> y end)
        pushed_rock = MapSet.new(falling)

        state(new_state,
          points: MapSet.union(points, pushed_rock),
          highest: max(highest, y_max),
          falling: nil
        )
      else
        new_state
      end

    next_state
  end

  def simulate(
        state(
          falling: falling,
          rocks_count: rocks_count
        ) = curr_state,
        jets,
        limit,
        rocks
      ) do
    if is_nil(falling) and rocks_count == limit do
      curr_state
    else
      if is_nil(falling) do
        drop_rock(curr_state, rocks)
      else
        fall(curr_state, jets)
      end
      |> simulate(jets, limit, rocks)
    end
  end

  def part1(input) do
    state(highest: highest) = simulate(state(), Stream.cycle(input), 2022, Stream.cycle(@rocks))

    highest
  end

  def part2(input) do
    # cycle_pattern(input, 1_000_000_000_000)
  end
end
