defmodule AdventOfCode.Day09 do
  @moduledoc """
  This one was cool! I got the problem fairly quickly, and was mostly debugging.

  Part 2 destroyed me, I hand checked every point both ways since I kept coming back
  1 short of the test input. Eventually I figured out that it was just a typo and I was
  updating the tail visits with the old state accumulator, so it was always one move behind.

  Programming, man.
  """

  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [dir, amount] = String.split(line)

      {case dir do
         "D" -> :down
         "U" -> :up
         "R" -> :right
         "L" -> :left
       end, String.to_integer(amount)}
    end)
  end

  @deltas %{down: {0, -1}, up: {0, 1}, right: {1, 0}, left: {-1, 0}}

  # if Euclidean distance less than or equal
  # to sqrt(2) then they are at most 1 away
  # including diagonally
  def is_touching(head, tail), do: euclidean_distance(head, tail) <= :math.sqrt(2)

  def move_head({{hx, hy}, tail}, direction) do
    {dx, dy} = @deltas[direction]
    {{hx + dx, hy + dy}, tail}
  end

  def move_tail({{hx, hy} = head, {tx, ty} = tail}) do
    unless is_touching(head, tail) do
      # clamp negative at -1 and divide by 
      # clamped positive to get the direction to move
      dx = Integer.floor_div(max(hx - tx, -1), max(hx - tx, 1))
      dy = Integer.floor_div(max(hy - ty, -1), max(hy - ty, 1))
      {head, {tx + dx, ty + dy}}
    else
      {head, tail}
    end
  end

  def part1(input) do
    initial_state = %{state: {{0, 0}, {0, 0}}, tail_visits: MapSet.new()}

    input
    |> Enum.reduce(initial_state, fn {dir, amount}, acc ->
      Enum.reduce(1..amount, acc, fn _, %{state: state} = inner_acc ->
        # move head and tail
        {head, tail} =
          move_head(state, dir)
          |> move_tail()

        Map.put(inner_acc, :state, {head, tail})
        |> Map.update!(:tail_visits, fn set -> MapSet.put(set, tail) end)
      end)
    end)
    |> Map.get(:tail_visits)
    |> MapSet.size()
  end

  def part2(input) do
    # positions are kept as a map now instead of a tuple
    knots = for x <- 0..9, do: {x, {0, 0}}, into: %{}

    initial_state = %{state: knots, tail_visits: MapSet.new()}

    input
    |> Enum.reduce(initial_state, fn {dir, amount}, acc ->
      Enum.reduce(1..amount, acc, fn _, %{state: state} = inner_acc ->
        # move the head of the rope
        new_state =
          move_head({state[0], state[1]}, dir)
          |> then(fn {moved_head, _} ->
            # update the head of the rope
            moved_state = Map.put(state, 0, moved_head)

            # go through each knot and move it relative to it's head
            Enum.reduce(0..8, moved_state, fn idx, state_acc ->
              head = Map.get(state_acc, idx)
              next = Map.get(state_acc, idx + 1)

              {_, moved_tail} = move_tail({head, next})
              Map.put(state_acc, idx + 1, moved_tail)
            end)
          end)

        Map.put(inner_acc, :state, new_state)
        # update tail visits to just position of the actual tail
        |> Map.update!(:tail_visits, fn set -> MapSet.put(set, new_state[9]) end)
      end)
    end)
    |> Map.get(:tail_visits)
    |> MapSet.size()
  end
end
