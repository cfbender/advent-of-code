defmodule AdventOfCode.Day20 do
  alias AdventOfCode.Helpers

  def its_lit(chars), do: Enum.map(chars, fn char -> if char == "#", do: :lit, else: :dark end)

  def parse_input(lines) do
    [algo | image] = lines

    algo_map = String.codepoints(algo) |> its_lit() |> Enum.with_index(&{&2, &1}) |> Map.new()

    pixels =
      Enum.map(image, fn line ->
        String.codepoints(line) |> its_lit()
      end)
      |> Helpers.list_to_map()

    {algo_map, pixels}
  end

  # pad two on all sides to check max distance from 3x3 window
  def pad(map, value \\ :dark, padding \\ 2) do
    points = Map.keys(map)
    {min_x, max_x} = Stream.map(points, &elem(&1, 0)) |> Enum.min_max()
    {min_y, max_y} = Stream.map(points, &elem(&1, 1)) |> Enum.min_max()

    for(
      x <- Range.new(min_x - padding, max_x + padding),
      y <- Range.new(min_y - padding, max_y + padding),
      Map.get(map, {x, y}) != :lit,
      do: {{x, y}, value}
    )
    |> Enum.into(%{})
    |> Map.merge(map)
  end

  def out_pixel(pixels, algo),
    # sort by ys first, default in elixir is to sort by first element of tuple
    do:
      Enum.sort_by(pixels, &elem(elem(&1, 0), 1))
      |> Stream.map(fn {_point, status} -> if status == :lit, do: 1, else: 0 end)
      |> Enum.join()
      |> Integer.parse(2)
      |> elem(0)
      |> then(fn x -> Map.get(algo, x) end)

  def enhance!(map, algo, times \\ 2, count \\ 0)
  def enhance!(map, _algo, times, count) when times == count, do: map

  def enhance!(map, algo, times, count) do
    # change default for alternating infinity
    default = if rem(count, 2) == 0, do: :dark, else: Map.get(algo, 0)

    pad(map, default)
    |> Stream.map(fn {point, _status} ->
      {point,
       Helpers.get_adj(map, point, default: default)
       |> out_pixel(algo)}
    end)
    |> Enum.into(%{})
    |> enhance!(algo, times, count + 1)
  end

  def part1(input) do
    {algo, pixels} = parse_input(input)

    pad(pixels)
    |> enhance!(algo)
    |> Enum.count(fn {_, status} -> status == :lit end)
  end

  def part2(input) do
    {algo, pixels} = parse_input(input)

    pad(pixels)
    |> enhance!(algo, 50)
    |> Enum.count(fn {_, status} -> status == :lit end)
  end
end
