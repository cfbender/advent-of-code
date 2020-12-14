defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "greets the world" do
    assert Aoc.main() == {2092, 702_970_661_767_766}
  end
end
