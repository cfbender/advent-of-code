defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "PART 1 calculates without parens" do
    assert "1 + 2 * 3 + 4 * 5 + 6" |> Aoc.flatten_eq() |> Aoc.process_equation() == "71"
  end

  test "PART 2 calculates with parens" do
    assert "1 + (2 * 3 + 2) + (4 * (5 + 6))" |> Aoc.flatten_eq() |> Aoc.process_equation() == "53"
  end
end
