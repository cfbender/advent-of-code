defmodule AdventOfCode.Day04 do
  # could probably use a better data structure to not make this O(n^3)
  defp mark_boards(boards, num) do
    Enum.map(boards, fn board ->
      Enum.map(board, fn line ->
        Enum.map(line, fn
          {number, _mark} when number == num -> {number, :marked}
          x -> x
        end)
      end)
    end)
  end

  defp check_board(board) do
    Enum.any?(board, fn line -> Enum.all?(line, fn {_num, status} -> status == :marked end) end)
  end

  defp check_boards(boards) do
    # boards plus pivoted boards to check row and column
    boards
    |> Enum.filter(fn board ->
      check_board(board) or
        Enum.zip(board)
        |> Enum.map(&Tuple.to_list/1)
        |> check_board()
    end)
  end

  defp play_bingo(calls, boards, win \\ true, previous_wins \\ []) do
    [call | rest_calls] = calls

    new_boards = mark_boards(boards, call)

    winning = check_boards(new_boards)

    cond do
      Enum.empty?(winning) ->
        play_bingo(rest_calls, new_boards, win, winning)

      length(winning) >= length(boards) ->
        IO.inspect(length(winning))
        IO.inspect(length(boards))

        {call,
         Enum.find(winning, fn board ->
           # get board that was not a previous win, mark them to check easily
           not Enum.member?(mark_boards(previous_wins, call), board)
         end)}

      win == false ->
        play_bingo(rest_calls, new_boards, win, winning)

      true ->
        {call, Enum.at(winning, 0)}
    end
  end

  def part1(input, win \\ true) do
    [calls | boards] = input

    calls =
      Enum.at(calls, 0)
      |> String.split(",", trim: true)
      |> Enum.map(&String.to_integer/1)

    IO.inspect(length(calls))

    boards =
      Enum.map(boards, fn board ->
        Enum.map(board, fn line ->
          String.split(line, " ", trim: true)
          |> Enum.map(fn num -> {String.to_integer(num), :unmarked} end)
        end)
      end)

    {final_call, winning_board} = play_bingo(calls, boards, win)

    sum_unmarked =
      Enum.reduce(winning_board, 0, fn line, acc ->
        marked =
          Stream.filter(line, fn {_num, status} -> status == :unmarked end)
          |> Stream.map(fn {num, _status} -> num end)
          |> Enum.sum()

        marked + acc
      end)

    final_call * sum_unmarked
  end

  def part2(input) do
    part1(input, false)
  end
end
