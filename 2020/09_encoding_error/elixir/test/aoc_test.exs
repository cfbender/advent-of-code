defmodule AocTest do
  use ExUnit.Case
  doctest AoC

  test "Parts" do
    assert AoC.run1() == 1_639_024_365
    assert AoC.run2() == 219_202_240
  end
end
