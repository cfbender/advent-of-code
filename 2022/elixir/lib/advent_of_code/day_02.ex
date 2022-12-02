defmodule AdventOfCode.Day02 do
  @moduledoc """
  Spent way too much time on this one avoiding the urge to hard code the plays for each win/lose condition.
  Kinda against what I put yesterday about the rules of AoC. Oh well!
  """
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      [a, b] = String.split(x, " ")

      {String.to_atom(a), String.to_atom(b)}
    end)
  end

  def points(:A, :Y), do: 6
  def points(:B, :Z), do: 6
  def points(:C, :X), do: 6
  def points(:A, :X), do: 3
  def points(:B, :Y), do: 3
  def points(:C, :Z), do: 3
  def points(_, _), do: 0

  def part1(input) do
    scores = %{X: 1, Y: 2, Z: 3}

    input
    |> Enum.map(fn {opp, me} ->
      scores[me] + points(opp, me)
    end)
    |> Enum.sum()
  end

  def part2(input) do
    scores = %{A: 1, B: 2, C: 3}
    conditions = %{X: 0, Y: 3, Z: 6}
    idx = %{A: 0, B: 1, C: 2}

    # 2 for 1 less than the cycle length so it loops back around
    diff = %{X: 2, Y: 0, Z: 1}

    cycle = Stream.cycle([:A, :B, :C])

    input
    |> Enum.map(fn {opp, me} ->
      play = Enum.at(cycle, idx[opp] + diff[me])
      conditions[me] + scores[play]
    end)
    |> Enum.sum()
  end
end
