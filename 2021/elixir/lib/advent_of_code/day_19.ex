defmodule AdventOfCode.Day19 do
  def parse_input(list) do
    Enum.reduce(list, [], fn lines, acc ->
      [data_line | beacons] = lines

      scanner =
        String.split(data_line, ["--- scanner ", " ---"], trim: true)
        |> List.first()
        |> String.to_integer()

      beacons =
        Enum.map(beacons, fn beacon ->
          String.split(beacon, ",") |> Enum.map(&String.to_integer/1)
        end)
        |> Enum.into(MapSet.new())

      [{scanner, beacons} | acc]
    end)
    |> Enum.into(%{})
  end

  def subtract_point([x, y, z], [i, j, k]), do: [x - i, y - j, z - k]
  def add_point([x, y, z], [i, j, k]), do: [x + i, y + j, z + k]

  def transform_scanner(scanner, xy, yz, xz) do
    Enum.reduce(0..xy, scanner, fn _curr, acc ->
      # rotate around z
      Enum.map(acc, fn [x, y, z] -> [-y, x, z] end)
    end)
    |> then(fn scanner ->
      Enum.reduce(0..yz, scanner, fn _curr, acc ->
        # rotate around x
        Enum.map(acc, fn [x, y, z] -> [x, z, -y] end)
      end)
    end)
    |> then(fn scanner ->
      Enum.reduce(0..xz, scanner, fn _curr, acc ->
        # rotate around y
        Enum.map(acc, fn [x, y, z] -> [z, y, -x] end)
      end)
    end)
  end

  def transform_scanner(scanner) do
    for x <- 0..3,
        y <- 0..3,
        z <- 0..3,
        do: transform_scanner(scanner, x, y, z)
  end

  def scanner_match?(a, b) do
    deltas = for l <- a, r <- b, do: subtract_point(l, r)
    {best, freq} = Enum.frequencies(deltas) |> Enum.max_by(&elem(&1, 1))
    if freq >= 12, do: best, else: nil
  end

  def find_match(a, rest) do
    Enum.find_value(rest, fn {x, b} ->
      match =
        transform_scanner(b)
        |> Enum.find_value(fn scanner_b ->
          delta = scanner_match?(a, scanner_b)
          if delta, do: {delta, scanner_b}
        end)

      if not is_nil(match), do: {x, match}, else: match
    end)
  end

  def merge_scanners(scanners, merged, deltas \\ [])
  def merge_scanners(scanners, merged, deltas) when map_size(scanners) == 0, do: {merged, deltas}

  def merge_scanners(scanners, merged, deltas) do
    # find scanner that matches with all combined points relative to scanner 0
    {match_x, {delta, match}} = find_match(merged, scanners)

    # combine points relative to first scanner matched (ie. add scanner 1 points relative to scanner zero)
    new_merged =
      Enum.map(match, fn point -> add_point(point, delta) end)
      |> Enum.reduce(merged, fn point, set -> MapSet.put(set, point) end)

    merge_scanners(Map.delete(scanners, match_x), new_merged, [delta | deltas])
  end

  def part1(input) do
    {first, scanners} = parse_input(input) |> Map.pop(0)

    {merged, _deltas} = merge_scanners(scanners, first)
    MapSet.size(merged)
  end

  def part2(input) do
    {first, scanners} = parse_input(input) |> Map.pop(0)

    {_merged, deltas} = merge_scanners(scanners, first)

    manhattans =
      for [px, py, pz] <- deltas,
          [qx, qy, qz] <- deltas,
          do: abs(px - qx) + abs(py - qy) + abs(pz - qz)

    Enum.max(manhattans)
  end
end
