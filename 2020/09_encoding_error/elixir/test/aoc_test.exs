defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "Parts" do
    assert Aoc.run1() == 1_639_024_365
    assert Aoc.run2() == 219_202_240
  end
end
