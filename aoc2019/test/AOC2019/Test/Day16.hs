module AOC2019.Test.Day16
  ( day16tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day16

day16tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 16 - Part 1 " $ TestList $ map (testExample day16a) examples1

part2Test :: Test
part2Test =
  TestLabel "Day 16 - Part 2 " $ TestList $ map (testExample day16b) examples2


examples1 :: [(String, String)]
examples1 =
  [ ("80871224585914546619083218645595", "24176176")
  , ("19617804207202209144916044189917", "73745418")
  , ("69317163492948606335995924319873", "52432133")
  ]

examples2 :: [(String, String)]
examples2 =
  [ ("03036732577212944063491565474664", "84462026")
  , ("02935109699940807407585447034323", "78725270")
  , ("03081770884921959731165446850517", "53553731")
  ]
