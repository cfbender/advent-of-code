defmodule AdventOfCode.Day17 do
  @input_regex ~r/target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)/

  def values_to_rest(init), do: Enum.scan(init..1, &(&1 + &2))

  def possible_x(min_x, max_x) do
    Enum.reduce(0..max_x, [], fn vx, acc ->
      if Enum.any?(values_to_rest(vx), &(&1 in min_x..max_x)) do
        [vx | acc]
      else
        acc
      end
    end)
  end

  def possible_y(min_y, max_y) do
    max_size = Enum.max([abs(min_y), abs(max_y)])

    Enum.reduce(min_y..max_size, [], fn vy, acc ->
      # get peak height if shot upwards initially, otherwise use vy and add one to adjust for the scan
      rest_height = if vy > 0, do: values_to_rest(vy) |> List.last(), else: vy + 1

      # scan through and find all y positions while falling from peak
      # give 1000 steps of space to check
      falling_ys =
        Stream.scan(1..1000, rest_height, &(&2 - &1))
        |> Enum.take_while(&(&1 >= min_y))

      if Enum.any?(falling_ys, &(&1 in min_y..max_y)) do
        [vy | acc]
      else
        acc
      end
    end)
  end

  def step({x, vx, y, vy}) do
    new_vx = if vx == 0, do: 0, else: if(vx > 0, do: vx - 1, else: vx + 1)
    {x + vx, new_vx, y + vy, vy - 1}
  end

  def land?(x, y, {min_x, max_x, min_y, max_y}) do
    Stream.iterate({0, x, 0, y}, &step/1)
    |> Enum.take_while(fn {x, dx, y, _dy} ->
      # probe has cleared target area by passing or falling past
      (x <= max_x and max_x >= 0 and dx > 0) or
        (x >= min_x and min_x <= 0 and dx < 0) or
        y >= min_y
    end)
    |> Enum.filter(fn {x, _, y, _} -> x in min_x..max_x and y in min_y..max_y end)
    |> Enum.any?()
  end

  def part1(input) do
    landing_data =
      {x1, x2, y1, y2} =
      Regex.run(@input_regex, input)
      |> Enum.drop(1)
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    xs = possible_x(x1, x2)

    # sorted by default via implementation
    possible_y(y1, y2)
    |> Enum.find(fn y -> Enum.any?(xs, fn x -> land?(x, y, landing_data) end) end)
    |> values_to_rest()
    |> List.last()
  end

  def part2(input) do
    landing_data =
      {x1, x2, y1, y2} =
      Regex.run(@input_regex, input)
      |> Enum.drop(1)
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    xs = possible_x(x1, x2)
    ys = possible_y(y1, y2)

    for(x <- xs, y <- ys, land?(x, y, landing_data), do: {x, y})
    |> Enum.count()
  end
end
