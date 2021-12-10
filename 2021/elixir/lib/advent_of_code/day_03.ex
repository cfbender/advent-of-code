defmodule AdventOfCode.Day03 do
  use Bitwise

  def part1(input) do
    gamma_binary =
      Enum.map(input, &String.codepoints/1)
      |> List.zip()
      |> Enum.map(&Tuple.to_list/1)
      |> Enum.map(fn list ->
        if Enum.count(list, fn x -> x == "1" end) > length(list) / 2, do: "1", else: "0"
      end)

    epsilon =
      Enum.join(gamma_binary)
      |> Integer.parse(2)
      |> Tuple.to_list()
      |> Enum.at(0)
      |> Bitwise.bxor(31)

    gamma =
      Enum.join(gamma_binary)
      |> Integer.parse(2)
      |> Tuple.to_list()
      |> Enum.at(0)

    gamma * epsilon
  end

  def find_rating([rating], _type, _position), do: rating

  def find_rating(ratings, type, position) do
    bit_frequency =
      Enum.map(ratings, fn rating -> Enum.at(rating, position) end)
      |> Enum.frequencies()

    {search, _num} =
      if type == "most",
        # return false to take ones as bit if equal
        do: Enum.max_by(bit_frequency, fn {_bit, count} -> count end, &>/2),
        else: Enum.min_by(bit_frequency, fn {_bit, count} -> count end)

    ratings
    |> Enum.filter(fn rating ->
      Enum.at(rating, position) == search
    end)
    |> find_rating(type, position + 1)
  end

  def part2(input) do
    bits = Enum.map(input, &String.codepoints/1)

    {oxygen, _rem} =
      bits
      |> find_rating("most", 0)
      |> Enum.join()
      |> Integer.parse(2)

    {co2, _rem} =
      bits
      |> find_rating("least", 0)
      |> Enum.join()
      |> Integer.parse(2)

    oxygen * co2
  end
end
