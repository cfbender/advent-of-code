defmodule AdventOfCode.Day08 do
  def part1(input) do
    Enum.map(input, fn line ->
      [_pattern, output] = String.split(line, "|", trim: true)

      String.split(output, " ", trim: true)
      |> Enum.count(fn x ->
        # one, four, seven and eight are only unique displays in count
        [2, 3, 4, 7]
        |> Enum.member?(String.length(x))
      end)
    end)
    |> Enum.sum()
  end

  def part2(input) do
    Enum.map(input, fn line ->
      [pattern, output] = String.split(line, "|", trim: true)

      [one, seven, four | rest] =
        String.split(pattern, " ", trim: true)
        |> Enum.sort_by(&byte_size/1)
        |> Enum.map(&(String.split(&1, "", trim: true) |> MapSet.new()))

      eight = List.last(rest)

      # two is the only 5 length display containing eight - seven - four
      two =
        Enum.find(rest, fn x ->
          MapSet.size(x) == 5 and
            MapSet.subset?(MapSet.difference(eight, seven) |> MapSet.difference(four), x)
        end)

      # three is the only display with one extra segment from the union of 2 and 1
      three =
        Enum.find(rest, fn x ->
          MapSet.size(MapSet.difference(MapSet.union(two, one), x)) == 1 and x !== two
        end)

      # five is the only 5 length display left
      five = Enum.find(rest, fn x -> MapSet.size(x) == 5 and x != two and x != three end)

      # six is only six length display where adding one (the display) completes it
      six = Enum.find(rest, fn x -> MapSet.union(x, one) == eight end)

      # nine is union of four and three
      nine = Enum.find(rest, fn x -> MapSet.union(four, three) == x end)

      # zero is the only one left
      zero =
        Enum.find(rest, fn x ->
          not Enum.member?([one, two, three, four, five, six, seven, eight, nine], x)
        end)

      nums = %{
        zero => 0,
        one => 1,
        two => 2,
        three => 3,
        four => 4,
        five => 5,
        six => 6,
        seven => 7,
        eight => 8,
        nine => 9
      }

      output
      |> String.split(" ", trim: true)
      |> Enum.map(&(String.split(&1, "", trim: true) |> MapSet.new()))
      |> Enum.map(fn x -> Map.get(nums, x) end)
      |> Enum.join()
      |> String.to_integer()
    end)
    |> Enum.sum()
  end
end
