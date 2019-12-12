module AOC2019.Test.Day1 where

import           AOC2019.Day1
import           AOC2019.Test.Common
import           Test.HUnit

day1tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 1 - Part 1 " $ TestList $ map (testExample day1a) examples1

part2Test :: Test
part2Test =
  TestLabel "Day 1 - Part 2 " $ TestList $ map (testExample day1b) examples2

examples1 :: [(String, Int)]
examples1 = [("12", 2), ("14", 2), ("1969", 654), ("100756", 33583)]

examples2 :: [(String, Int)]
examples2 = [("14", 2), ("1969", 966), ("100756", 50346)]
