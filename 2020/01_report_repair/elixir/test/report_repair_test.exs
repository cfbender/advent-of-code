defmodule AoCTest do
  use ExUnit.Case
  doctest AoC

  test "Parts" do
    assert AoC.run1() == 935_419
    assert AoC.run2() == 49_880_012
  end
end
