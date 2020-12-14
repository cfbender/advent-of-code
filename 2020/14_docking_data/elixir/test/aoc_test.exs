defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "gets memory and address" do
    assert Aoc.get_mem_ins("mem[69] = 420") == {69, 420}
  end

  test "masks bits" do
    assert Aoc.mask_bits("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", 11) == 73
  end

  test "replaces xs with list" do
    assert Aoc.replace_x([0, 1], "000000000000000000000000000000X1001X") ==
             "000000000000000000000000000000010011"
  end

  test "generates all floating insertions" do
    assert Aoc.get_floating_writes("000000000000000000000000000000X1001X", 42) == [26, 27, 58, 59]
  end

  test "gets the right answers" do
    assert Aoc.main() == {17_765_746_710_228, 4_401_465_949_086}
  end
end
