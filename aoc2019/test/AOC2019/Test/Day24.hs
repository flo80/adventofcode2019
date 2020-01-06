module AOC2019.Test.Day24
  ( day24tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day24

day24tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 24 - Part 1 " $ TestList $ map (testExample day24a) examples1

part2Test :: Test
part2Test = TestLabel "Day 24 - Part 2 " $ TestList $ map
  (testExample $ day24b 10)
  examples2


examples1 :: [(String, Int)]
examples1 = [("....#\n#..#.\n#..##\n..#..\n#....\n", 2129920)]

examples2 :: [(String, Int)]
examples2 = [("....#\n#..#.\n#..##\n..#..\n#....\n", 99)]
