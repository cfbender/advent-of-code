defmodule AdventOfCode.Day15 do
  @moduledoc """
  Part 1 easy enough, but another where you have to think of a clever way to solve part 2.

  Not bad, but I couldn't figure it out and had to look up a hint to set me on the perimeter path.
  This is sloooow (~2 min on my laptop), but it works! Could definitely optimize the data structures I think.

  Edit: I came back and parallelized this and got it under 30 seconds. good enough!
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

  def perimeter({{sx, sy}, distance}) do
    x_range = (sx - distance - 1)..sx

    Enum.with_index(x_range)
    |> Enum.chunk_every(100)
    |> Enum.map(fn xs ->
      Task.async(fn ->
        Enum.reduce(xs, [], fn {x, slope}, list ->
          [
            {x, sy + slope},
            {x, sy - slope},
            reflect_x({x, sy + slope}, {sx, sy}),
            reflect_x({x, sy + slope}, {sx, sy}) |> reflect_y({sx, sy})
            | list
          ]
        end)
      end)
    end)
    |> Task.await_many()
    |> List.flatten()
    |> List.flatten()
  end

  def perimeters(sensors) do
    Enum.map(sensors, fn sensor ->
      Task.async(fn -> perimeter(sensor) end)
    end)
    |> Task.await_many(10_000)
    |> List.flatten()
  end

  def part2(input, bound) do
    {sensors, _beacons} = input
    range = 0..bound

    {x, y} =
      perimeters(sensors)
      |> Enum.chunk_every(100)
      |> Enum.map(fn points ->
        Task.async(fn ->
          Enum.find(points, fn {x, y} = point ->
            x in range and
              y in range &&
              Enum.all?(sensors, fn {sensor, distance} ->
                manhattan_distance(sensor, point) > distance
              end)
          end)
        end)
      end)
      |> Task.await_many()
      |> Enum.find(fn x -> not is_nil(x) end)

    x * 4_000_000 + y
  end
end
