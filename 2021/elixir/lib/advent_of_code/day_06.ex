defmodule AdventOfCode.Day06 do
  def record_fish(fish_list) do
    nums = for x <- 0..8, into: %{}, do: {x, 0}
    Enum.reduce(fish_list, nums, fn x, acc -> Map.update(acc, x, 1, &(&1 + 1)) end)
  end

  def track_fish(fish, final, count \\ 0)
  def track_fish(fish, final, count) when count == final, do: fish

  def track_fish(fish, final, count) do
    new_fish =
      for x <- 0..7,
          into: %{},
          # get fish from one timer higher
          do: {x, Map.get(fish, x + 1)}

    new_fish
    # add zeroes to sixes
    |> Map.update(6, 0, &(&1 + Map.get(fish, 0)))
    # move all zeroes to eights
    |> Map.put(8, Map.get(fish, 0))
    |> track_fish(final, count + 1)
  end

  def part1(input) do
    record_fish(input)
    |> track_fish(80)
    |> Map.values()
    |> Enum.sum()
  end

  def part2(input) do
    record_fish(input)
    |> track_fish(256)
    |> Map.values()
    |> Enum.sum()
  end
end
