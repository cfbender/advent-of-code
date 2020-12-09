defmodule ReportRepairTest do
  use ExUnit.Case

  test "Parts" do
    assert ReportRepair.run1() == 935_419
    assert ReportRepair.run2() == 49_880_012
  end
end
