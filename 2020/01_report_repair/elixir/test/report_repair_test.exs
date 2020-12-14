defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "Parts" do
    assert Aoc.run1() == 935_419
    assert Aoc.run2() == 49_880_012
  end
end
