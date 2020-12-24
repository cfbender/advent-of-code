defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "parses directions properly" do
    assert Aoc.movement("n") == {0, 1}
    assert Aoc.movement("sw") == {-0.5, -0.5}
  end

  test "finds correct position" do
    assert ["e", "e", "w", "ne"] |> Aoc.find_pos({0, 0}) == {1.5, 0.5}
  end

  test "finds adjacent tiles" do
    assert {1, 1} |> Aoc.find_adj() == [
             {1.5, 1.5},
             {1.5, 0.5},
             {0.5, 1.5},
             {0.5, 0.5},
             {2, 1},
             {0, 1}
           ]
  end

  test "pads map" do
    assert %{{0, 0} => 0} |> Aoc.pad_map() == %{
             {0, 0} => 0,
             {0.5, 0.5} => 1,
             {1, 0} => 1,
             {0.5, -0.5} => 1,
             {-0.5, -0.5} => 1,
             {-1, 0} => 1,
             {-0.5, 0.5} => 1
           }
  end

  test "answers correctly" do
    assert Aoc.main() == {287, 3636}
  end
end
