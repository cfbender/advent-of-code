defmodule Aoc do
  def get_input do
    File.read!("../input.txt") |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
  end

  @divisor 20_201_227

  def crypto_loop(num, _subj, count, limit) when count == limit, do: num

  def crypto_loop(num, subj, count, limit) do
    rem(num * subj, @divisor) |> crypto_loop(subj, count + 1, limit)
  end

  def find_loop_size(num, target, curr) do
    result = crypto_loop(num, 7, 0, 1)

    cond do
      result == target ->
        # return 0 indexed loop counts
        curr + 1

      true ->
        # carry result value to do next loop calc
        find_loop_size(result, target, curr + 1)
    end
  end

  def main do
    input = get_input() |> IO.inspect()

    # start with 1, find loop size
    [_card_loops, door_loops] = input |> Enum.map(&find_loop_size(1, &1, 0))

    # run card key with door loops amount
    card_key = input |> Enum.at(0)
    crypto_loop(1, card_key, 0, door_loops)
  end
end
