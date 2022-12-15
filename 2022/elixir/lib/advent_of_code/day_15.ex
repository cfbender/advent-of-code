defmodule AdventOfCode.Day15 do
  @moduledoc """
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.reduce({%{}, MapSet.new()}, fn line, {sensors, beacons} ->
      [sx, sy, bx, by] =
        Regex.run(~r/Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)/, line)
        |> Enum.drop(1)
        |> Enum.map(&String.to_integer/1)

      {Map.put(sensors, {sx, sy}, manhattan_distance({sx, sy}, {bx, by})),
       MapSet.put(beacons, {bx, by})}
    end)
  end

  def scanned?(sensors, point) do
    Enum.any?(sensors, fn {sensor, distance} ->
      manhattan_distance(sensor, point) <= distance
    end)
  end

  def part1(input, row) do
    {sensors, beacons} = input
    {{{x_min, _}, _}, {{x_max, _}, _}} = Enum.min_max_by(sensors, fn {{x, _}, _} -> x end)
    {_, max_distance} = Enum.max_by(sensors, &elem(&1, 1))
    range = (x_min - max_distance)..(x_max + max_distance)

    Enum.count(range, fn x ->
      scanned?(sensors, {x, row}) and not MapSet.member?(beacons, {x, row})
    end)
  end

  def map_scanned(sensors) do
    Enum.reduce(sensors, MapSet.new(), fn {{sx, sy} = sensor, distance}, set ->
      x_range = (sx - distance)..(sx + distance)
      y_range = (sy - distance)..(sy + distance)

      Enum.reduce(y_range, set, fn y, y_acc ->
        Enum.reduce(x_range, y_acc, fn x, x_acc ->
          if manhattan_distance({x, y}, sensor) <= distance,
            do: MapSet.put(x_acc, {x, y}),
            else: x_acc
        end)
        |> MapSet.union(y_acc)
      end)
    end)
  end

  def part2(input, bound) do
    {sensors, _beacons} = input
    range = 0..bound

    scanned = map_scanned(sensors)
    dbg("done mapping")

    {x, y} =
      Enum.find_value(range, fn y ->
        val =
          Enum.find(range, fn x ->
            not MapSet.member?(scanned, {x, y})
          end)

        if val, do: {val, y}, else: nil
      end)

    x * 4_000_000 + y
  end
end
