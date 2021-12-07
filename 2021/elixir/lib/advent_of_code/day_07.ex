defmodule AdventOfCode.Day07 do
  def part1(input) do
    sorted = Enum.sort(input)
    median = Enum.at(sorted, Integer.floor_div(length(input), 2))

    Enum.reduce(sorted, 0, &(abs(&1 - median) + &2))
  end

  def move_crabs(crabs, destination, previous \\ Infinity) do
    cost =
      Enum.reduce(crabs, 0, fn curr, acc ->
        distance = abs(curr - destination)
        # (n^2 + n) / 2 (nth triangle number)
        acc + Integer.floor_div(Integer.pow(distance, 2) + distance, 2)
      end)

    if(cost > previous) do
      previous
    else
      move_crabs(crabs, destination + 1, cost)
    end
  end

  def part2(input) do
    sorted = Enum.sort(input)
    median = Enum.at(sorted, Integer.floor_div(length(input), 2))

    move_crabs(sorted, median)
  end
end
