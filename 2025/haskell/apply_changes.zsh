#!/bin/zsh

for x in "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"; do cat src/Days/Day01.hs | sed "s/01/$x/" >src/Days/Day"$x".hs; done
for x in "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"; do cat test/Days/Day01Spec.hs | sed "s/01/$x/" >test/Days/Day"$x"Spec.hs; done
