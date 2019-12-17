module AOC2019.Test.Day17
  ( day17tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day17

day17tests = TestList [part1Test]--, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 17 - Part 1 " $ TestList $ map (testExample day17a) examples1

part2Test :: Test
part2Test =
  TestLabel "Day 17 - Part 2 " $ TestList $ map (testExample day17b) examples2


examples1 :: [([Int], Int)]
examples1 =
  [ ( map
      fromEnum
      "..#..........\n..#..........\n#######...###\n#.#...#...#.#\n#############\n..#...#...#..\n..#####...^..\n"
    , 76
    )
  ]

examples2 :: [(String, Int)]
examples2 = undefined

