module AOC2019.Test.Day6
  ( day6tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day6

day6tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 6 - Part 1 " $ TestList $ map (testExample day6a) examples1

part2Test :: Test
part2Test =
  TestLabel "Day 6 - Part 2 " $ TestList $ map (testExample day6b) examples2


examples1 :: [(String, Int)]
examples1 = [("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L",42)]

examples2 :: [(String, Int)]
examples2 = [("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n",4)]
