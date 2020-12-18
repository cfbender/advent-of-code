defmodule Aoc do
  def get_input do
    {:ok, input} = File.read("../input.txt")

    input |> String.split("\n", trim: true)
  end

  def process_equation([], start, _), do: start

  def process_equation(equation) do
    # part 1:
    # if equation |> String.contains?("*") or equation |> String.contains?("+") do
    #   ~r"(\d+)\s*[\*,\+]\s*(\d+)"
    #   |> Regex.replace(
    #     equation,
    #     fn match, x, y ->
    #       if match |> String.contains?("*"),
    #         do: ((x |> String.to_integer()) * (y |> String.to_integer())) |> Integer.to_string(),
    #         else: ((x |> String.to_integer()) + (y |> String.to_integer())) |> Integer.to_string()
    #     end,
    #     global: false
    #   )
    #   |> process_equation()

    if equation |> String.contains?("+") do
      ~r"(\d+)\s*\+\s*(\d+)"
      |> Regex.replace(
        equation,
        fn _, x, y ->
          ((x |> String.to_integer()) + (y |> String.to_integer())) |> Integer.to_string()
        end,
        global: false
      )
      |> process_equation()
    else
      if equation |> String.contains?("*") do
        ~r"(\d+)\s*\*\s*(\d+)"
        |> Regex.replace(
          equation,
          fn _, x, y ->
            ((x |> String.to_integer()) * (y |> String.to_integer())) |> Integer.to_string()
          end,
          global: false
        )
        |> process_equation()
      else
        equation
      end
    end
  end

  def flatten_eq(equation) do
    if equation |> String.contains?("(") do
      ~r"\(([^()]+)\)"
      |> Regex.replace(
        equation,
        fn _, match ->
          match |> process_equation()
        end,
        global: false
      )
      |> flatten_eq()
    else
      equation
    end
  end

  def main do
    input = get_input()

    input
    |> Enum.map(fn line -> line |> flatten_eq |> process_equation() |> String.to_integer() end)
    |> Enum.sum()
  end
end
