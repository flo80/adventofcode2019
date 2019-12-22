module AOC2019.Test.Day22
  ( day22tests
  )
where

import           Test.HUnit
import           AOC2019.Test.Common
import           AOC2019.Day22

day22tests = TestList [indivTests, groupTests]

indivTests :: Test
indivTests =
  TestLabel "Day 22 - Indiv. Shuffles "
    $ TestList
    $ (  map (\(n, f, i, o) -> testFunction n f i o) examplesIndiv
      ++ map
           (\(i, o) -> testFunction
             i
             (\input -> shuffle [0 .. 9] (parseInput input))
             i
             o
           )
           examplesGroup
      )


groupTests :: Test
groupTests =
  TestLabel "Day 22 - Indiv. Shuffles "
    $ TestList
    $ (  map (\(n, f, i, o) -> testFunction n f i o) examplesIndiv
      ++ map
            (\(i, o) -> testFunction
              i
              (\input -> shuffle [0 .. 9] (parseInput input))
              i
              o
            )
            examplesGroup
      )


examplesIndiv :: [(String, ([Int] -> [Int]), [Int], [Int])]
examplesIndiv =
  [ ( "dealIntoNewStack"
    , dealIntoNewStack
    , [0 .. 9]
    , [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
    )
  , ("cutNCards 3" , cutNCards 3   , [0 .. 9], [3, 4, 5, 6, 7, 8, 9, 0, 1, 2])
  , ("cutNCards -4", cutNCards (-4), [0 .. 9], [6, 7, 8, 9, 0, 1, 2, 3, 4, 5])
  , ( "dealWithIncrementN 3"
    , dealWithIncrementN 3
    , [0 .. 9]
    , [0, 7, 4, 1, 8, 5, 2, 9, 6, 3]
    )
  ]

  
examplesGroup :: [(String, [Int])]
examplesGroup =
  [ ( "deal with increment 7\ndeal into new stack\ndeal into new stack\n"
    , [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
    )
  , ("cut 6\ndeal with increment 7\ndeal into new stack\n",[3, 0, 7, 4, 1, 8, 5, 2, 9, 6])
  , ("deal with increment 7\ndeal with increment 9\ncut -2\n",[6, 3, 0, 7, 4, 1, 8, 5, 2, 9]  )
  , ("deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1\n", [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]  )
  ]
