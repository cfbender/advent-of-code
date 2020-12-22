defmodule Aoc do
  def get_input do
    input = File.read!("../input.txt")
    decks = input |> String.split("\n\n", trim: true)

    deck_regex = ~r/Player (\d):(.*)/s

    decks
    |> Enum.map(fn deck ->
      [_, player, deck] = deck_regex |> Regex.run(deck)

      {player, deck |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)}
    end)
  end

  def play(p1, []), do: p1
  def play([], p2), do: p2

  def play(p1, p2) do
    [p1_draw | p1_rest] = p1
    [p2_draw | p2_rest] = p2

    cond do
      p1_draw > p2_draw ->
        play(p1_rest |> Enum.concat([p1_draw, p2_draw]), p2_rest)

      true ->
        play(p1_rest, p2_rest |> Enum.concat([p2_draw, p1_draw]))
    end
  end

  def recursive_play(p1, [], _, _), do: {p1, []}
  def recursive_play([], p2, _, _), do: {[], p2}

  def recursive_play(p1, p2, p1_hist, p2_hist) do
    cond do
      p1_hist |> MapSet.member?(p1) or p2_hist |> MapSet.member?(p2) ->
        {p1, []}

      true ->
        # new combination, continue game
        [p1_draw | p1_rest] = p1
        [p2_draw | p2_rest] = p2

        p1_next_hist = p1_hist |> MapSet.put(p1)
        p2_next_hist = p2_hist |> MapSet.put(p2)

        result =
          cond do
            p1_draw <= length(p1_rest) and p2_draw <= length(p2_rest) ->
              # enter sub-game
              {p1_sub, p2_sub} =
                recursive_play(
                  p1_rest |> Enum.take(p1_draw),
                  p2_rest |> Enum.take(p2_draw),
                  p1_next_hist,
                  p2_next_hist
                )

              # use result of sub game for this round
              {length(p1_sub) > 0, length(p2_sub) > 0}

            true ->
              {p1_draw > p2_draw, p2_draw > p1_draw}
          end

        case result do
          # P1 round win
          {true, false} ->
            recursive_play(
              p1_rest |> Enum.concat([p1_draw, p2_draw]),
              p2_rest,
              p1_next_hist,
              p2_next_hist
            )

          # P2 round win
          {false, true} ->
            recursive_play(
              p1_rest,
              p2_rest |> Enum.concat([p2_draw, p1_draw]),
              p1_next_hist,
              p2_next_hist
            )
        end
    end
  end

  def main do
    [{_, p1_deck}, {_, p2_deck}] = get_input()

    part1 =
      play(p1_deck, p2_deck)
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.reduce(0, fn {card, idx}, acc -> acc + (idx + 1) * card end)

    part2 =
      recursive_play(p1_deck, p2_deck, MapSet.new(), MapSet.new())
      |> Tuple.to_list()
      |> List.flatten()
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.reduce(0, fn {card, idx}, acc -> acc + (idx + 1) * card end)

    {part1, part2}
  end
end
