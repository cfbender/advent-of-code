defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "gets the right answers" do
    assert Aoc.main() == {32366, 30891}
  end
end
