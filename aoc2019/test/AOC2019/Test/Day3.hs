module AOC2019.Test.Day3
  ( day3tests
  )
where


import           Test.HUnit
import           AOC2019.Day3

day3tests = TestList [part1Test, part2Test]

part1Test = TestLabel "Day 3 - Part 1 " $ TestList $ map testExample examples
 where
  testExample (a, b, o, _) =
    TestCase (assertEqual (show o) (day3a (a ++ "\n" ++ b)) o)

examples :: [(String, String, Int, Int)]
examples =
  [ ( "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    , "U62,R66,U55,R34,D71,R55,D58,R83"
    , 159
    , 610
    )
  , ( "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    , 135
    , 410
    )
  ]

part2Test = TestLabel "Day 3 - Part 2 " $ TestList $ map testExample examples
 where
  testExample (a, b, _, o) =
    TestCase (assertEqual (show o) (day3b (a ++ "\n" ++ b)) o)
