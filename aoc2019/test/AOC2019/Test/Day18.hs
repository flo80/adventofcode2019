module AOC2019.Test.Day18
  ( day18tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day18

day18tests = TestList [part1Test]--, part2Test]

part1Test :: Test
part1Test =
  TestLabel "Day 18 - Part 1 " $ TestList $ map (testExample day18a) examples1

part2Test :: Test
part2Test =
  TestLabel "Day 18 - Part 2 " $ TestList $ map (testExample day18b) examples2


examples1 :: [(String, Int)]
examples1 =
  [ ("#########\n#b.A.@.a#\n#########\n", 8)
  , ( "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################\n"
    , 86
    )
  , ( "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################\n"
    , 132
    )
  , ( "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################\n"
    , 136
    )
  , ( "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################\n"
    , 81
    )
  ]

examples2 :: [(String, Int)]
examples2 = undefined

