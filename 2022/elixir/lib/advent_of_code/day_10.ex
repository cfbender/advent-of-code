defmodule AdventOfCode.Day10 do
  @moduledoc """
  This one hurt. Misread the problem initially and took 2.5 hours to realize it.
  I was assuming it took one instruction per cycle and implemented a queue to handle
  pending operations in separate threads that would get applied in each cycle when ready.

  Yeah, no. It was just that addx took 2 cycles. Rewrote it from scratch and got it quickly 
  after that at least (at 3 AM). Really gotta make sure I understand it from now on.
  """
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [instruction | tail] = String.split(line)
      instruction = String.to_atom(instruction)

      if length(tail) > 0 do
        {instruction, String.to_integer(List.first(tail))}
      else
        {instruction}
      end
    end)
  end

  def interesting?(count), do: count == 20 or rem(count - 20, 40) == 0

  def check_signal(state) do
    if interesting?(state.cycle) do
      Map.update!(state, :signal, fn list -> [state.cycle * state.register | list] end)
    else
      state
    end
  end

  def instruction({:addx, x}, state) do
    state
    |> check_signal()
    |> Map.put(:pending, x)
  end

  def instruction({:noop}, state),
    do: check_signal(state)

  def apply_pending(state),
    do:
      Map.update!(state, :register, fn register -> register + state.pending end)
      |> Map.put(:pending, nil)

  # state shape: %{register: number(), cycle: number(), pending: number | nil, signal: [number()]} 
  def cycle(instructions, state, opts \\ [])
  def cycle([], %{pending: nil} = state, _opts), do: state

  def cycle([], state, opts) do
    draw = opts[:draw]
    if draw, do: draw(state)
    apply_pending(state)
  end

  def cycle(instructions, %{pending: pending} = state, opts) do
    draw = opts[:draw]
    if draw, do: draw(state)

    if is_nil(pending) do
      [i | tail] = instructions
      state = instruction(i, state) |> Map.update!(:cycle, &(&1 + 1))
      cycle(tail, state, opts)
    else
      state =
        check_signal(state)
        |> apply_pending()
        |> Map.update!(:cycle, &(&1 + 1))

      cycle(instructions, state, opts)
    end
  end

  def draw(state) do
    sprite = for x <- -1..1, do: x + state.register
    cursor = rem(state.cycle - 1, 40)
    out = if cursor in sprite, do: "#", else: "."
    IO.write(out)
    if cursor == 39, do: IO.write("\n")
  end

  def part1(input) do
    input
    |> cycle(%{register: 1, cycle: 1, pending: nil, signal: []})
    |> Map.get(:signal)
    |> Enum.sum()
  end

  def part2(input) do
    input
    |> cycle(%{register: 1, cycle: 1, pending: nil, signal: []}, draw: true)

    IO.puts("")
  end
end
