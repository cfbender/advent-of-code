defmodule AoCTest do
  use ExUnit.Case
  doctest AoC

  test "greets the world" do
    assert AoC.main() == {2483, 2285}
  end
end
