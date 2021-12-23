defmodule AdventOfCode.Day22 do
  def string_to_range(string),
    do:
      String.split(string, "..")
      |> Enum.map(&String.to_integer/1)
      # add one to help calculate overlaps
      |> then(fn [a, b] -> Range.new(a, b + 1) end)

  def parse_input(input) do
    Enum.map(input, fn line ->
      [status, x, y, z] = String.split(line, [" x=", ",y=", ",z="])

      [String.to_atom(status) | Enum.map([x, y, z], &string_to_range/1)]
      |> List.to_tuple()
    end)
  end

  def empty([x, y, z]), do: x.last <= x.first or y.last <= y.first or z.last <= z.first

  def intersection(cube1, cube2) do
    Enum.zip(cube1, cube2)
    |> Enum.map(fn {a, b} ->
      if Range.disjoint?(a, b) do
        Range.new(0, 0)
      else
        {minA, maxA} = Enum.min_max(a)
        {minB, maxB} = Enum.min_max(b)

        Range.new(max(minA, minB), min(maxA, maxB))
      end
    end)
  end

  def difference(cube1 = {cube, children}, cube2) do
    cropped = intersection(cube2, cube)

    if empty(cropped),
      do: cube1,
      else: {cube, [{cropped, []} | Enum.map(children, &difference(&1, cropped))]}
  end

  def out_initial?(range), do: Range.disjoint?(range, -50..50)

  def process_steps(steps, cubes \\ [])
  def process_steps([], cubes), do: cubes

  def process_steps([{type, x_r, y_r, z_r} | rest], cubes) do
    curr_cube = [x_r, y_r, z_r]

    # difference the current cube. if it's on - prevents from being double counted. it off, removes it from all
    new_cubes = Enum.map(cubes, &difference(&1, curr_cube))

    if type == :on do
      process_steps(rest, [{curr_cube, []} | new_cubes])
    else
      process_steps(rest, new_cubes)
    end
  end

  def volume({cube, children}), do: volume(cube) - Enum.sum(Enum.map(children, &volume/1))

  def volume(bounds),
    # subtract one from the original parsing
    do:
      Enum.map(bounds, fn bound -> Range.size(bound) - 1 end)
      |> Enum.product()

  def part1(input) do
    parse_input(input)
    |> Enum.filter(fn {_, x, y, z} -> not Enum.any?([x, y, z], fn x -> out_initial?(x) end) end)
    |> process_steps()
    |> Enum.map(&volume/1)
    |> Enum.sum()
  end

  def part2(input) do
    parse_input(input)
    |> process_steps()
    |> Enum.map(&volume/1)
    |> Enum.sum()
  end
end
