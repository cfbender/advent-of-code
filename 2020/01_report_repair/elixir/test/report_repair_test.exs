defmodule ReportRepairTest do
  use ExUnit.Case

  test "Parts" do
    assert AoC.run1() == 935_419
    assert AoC.run2() == 49_880_012
  end
end
