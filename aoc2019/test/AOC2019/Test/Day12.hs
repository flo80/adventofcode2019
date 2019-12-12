module AOC2019.Test.Day12
  ( day12tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day12

day12tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 12 - Part 1 " $ TestList $ map (testExample day12a) examples1

part2Test :: Test
part2Test =
  TestLabel "Day 12 - Part 2 " $ TestList $ map (testExample day12b) examples2


-- Input String, Number of steps, resulting energy
examples1 :: [((String, Int), Int)]
examples1 =
  [ ( ( "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>\n"
      , 10
      )
    , 179
    )
  , ( ( "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>\n"
      , 100
      )
    , 1940
    )
  ]


examples2 :: [(String, Int)]
examples2 =
  [ ( "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>\n"
    , 2772
    )
  , ( "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>\n"
    , 4686774924
    )
  ]
