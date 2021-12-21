defmodule AdventOfCode.Day21 do
  defmodule Game do
    defstruct p1: {0, 0}, p2: {0, 0}, last_roll: 0, count_rolls: 0

    def new([p1, p2]) do
      %Game{p1: {p1, 0}, p2: {p2, 0}}
    end
  end

  def get_next_rolls(last_roll), do: Stream.cycle(1..100) |> Enum.slice(last_roll, 3)

  def play(%Game{p1: {_, p1_score}, p2: {_, p2_score}, count_rolls: count_rolls}, _turn)
      when p1_score >= 1000 or p2_score >= 1000,
      do: Enum.min([p1_score, p2_score]) * count_rolls

  def play(
        %Game{
          last_roll: last_roll,
          count_rolls: count_rolls
        } = input,
        turn
      ) do
    board = Stream.cycle(1..10)
    next = if turn == :p1, do: :p2, else: :p1
    {curr_pos, curr_score} = Map.get(input, turn, 0)

    roll = get_next_rolls(last_roll)
    last_roll = List.last(roll)
    roll = Enum.sum(roll)

    new_pos = Enum.at(board, roll + curr_pos - 1)
    data = {new_pos, curr_score + new_pos}

    Map.merge(input, %{last_roll: last_roll, count_rolls: count_rolls + 3})
    |> Map.put(turn, data)
    |> play(next)
  end

  @spec sum_scores({number, number}, {number, number}) :: {number, number}
  def sum_scores({a, b}, {x, y}), do: {a + x, b + y}

  @quantum_rolls for i <- 1..3, j <- 1..3, k <- 1..3, do: i + j + k

  def play_quantum(input, turn \\ :p1, cache \\ %{})

  def play_quantum(%Game{p1: {_, p1_score}, p2: {_, p2_score}}, _turn, _memo)
      when p1_score >= 21 or p2_score >= 21,
      do: if(p1_score > p2_score, do: {1, 0}, else: {0, 1})

  def play_quantum(input, turn, memo) do
    case Agent.get(memo, &Map.get(&1, {input, turn})) do
      nil ->
        board = Stream.cycle(1..10)

        {curr_pos, curr_score} = Map.get(input, turn, 0)
        next = if turn == :p1, do: :p2, else: :p1

        Enum.map(@quantum_rolls, fn roll ->
          new_pos = Enum.at(board, roll + curr_pos - 1)
          data = {new_pos, curr_score + new_pos}

          new_input = Map.put(input, turn, data)
          result = play_quantum(new_input, next, memo)

          Agent.update(memo, &Map.put(&1, {new_input, next}, result))
          result
        end)
        |> Enum.reduce(&sum_scores/2)

      result ->
        result
    end
  end

  def part1(input) do
    Game.new(input)
    |> play(:p1)
  end

  def part2(input) do
    {:ok, memo} = Agent.start(fn -> %{} end)

    Game.new(input)
    |> play_quantum(:p1, memo)
    |> Tuple.to_list()
    |> Enum.max()
  end
end
