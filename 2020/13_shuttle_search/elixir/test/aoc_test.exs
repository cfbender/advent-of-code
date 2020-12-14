defmodule AoCTest do
  use ExUnit.Case
  doctest AoC

  test "greets the world" do
    assert AoC.main() == {2092, 702_970_661_767_766}
  end
end
