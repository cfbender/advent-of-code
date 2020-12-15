defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "gets correct numbers" do
    assert Aoc.main() == {700, 51358}
  end
end
