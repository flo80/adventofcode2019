module AOC2019.Test.IntCodeComputer
  ( computerTests
  )
where

import           Test.HUnit
import           AOC2019.IntCodeComputer
import           Data.Foldable                            ( toList )


computerTests = TestList [testExamplesWithCode, testExamplesIO]


testExamplesWithCode :: Test
testExamplesWithCode = TestLabel "IntCode Computer - Memory " $ TestList $ map
  test
  examplesWithCode
 where
    -- ((Memory, Input),(Memory, Output))
  test ((im, ii), (om, oo)) = TestList
    [ TestCase (assertEqual ("Memory " ++ show im) om mem)
    , TestCase (assertEqual ("Output " ++ show im) oo out)
    ]
   where
    (mem, out) = (toList $ memory c, output c)
    c          = runComputer $ newComputer im ii


testExamplesIO :: Test
testExamplesIO = TestLabel "IntCode Computer - Output " $ TestList $ map
  test
  examplesIO
 where
  test (im, ii, oo) = TestCase (assertEqual (show im) oo out)
    where out = output $ runComputer $ newComputer im ii


examplesWithCode :: [((String, [Int]), ([Int], [Int]))]
examplesWithCode =
  [ (("1,0,0,0,99", [])         , ([2, 0, 0, 0, 99], []))
  , (("2,3,0,3,99", [])         , ([2, 3, 0, 6, 99], []))
  , (("2,4,4,5,99,0", [])       , ([2, 4, 4, 5, 99, 9801], []))
  , (("1,1,1,4,99,5,6,0,99", []), ([30, 1, 1, 4, 2, 5, 6, 0, 99], []))
  , ( ("1,9,10,3,2,3,11,0,99,30,40,50"               , [])
    , ([3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50], [])
    )
  ,
  -- input output test
    (("3,0,4,0,99", [0])    , ([0, 0, 4, 0, 99], [0]))
  , (("3,0,4,0,99", [-1])   , ([-1, 0, 4, 0, 99], [-1]))
  ,
  -- test rewriting own code
    (("1101,100,-1,4,0", []), ([1101, 100, -1, 4, 99], []))
  ]


-- Program, Input, Output
examplesIO :: [(String, [Int], [Int])]
examplesIO =
  -- position mode - eq 8
  [ ("3,9,8,9,10,9,4,9,99,-1,8", [8], [1])
  , ("3,9,8,9,10,9,4,9,99,-1,8", [7], [0])
  , ( "3,9,8,9,10,9,4,9,99,-1,8"
    , [9]
    , [0]
    )
  -- position mode - lt 8
  , ("3,9,7,9,10,9,4,9,99,-1,8", [9], [0])
  , ("3,9,7,9,10,9,4,9,99,-1,8", [8], [0])
  , ( "3,9,7,9,10,9,4,9,99,-1,8"
    , [7]
    , [1]
    )
  -- immediate mode - eq 8
  , ("3,3,1108,-1,8,3,4,3,99", [8], [1])
  , ("3,3,1108,-1,8,3,4,3,99", [7], [0])
  , ( "3,3,1108,-1,8,3,4,3,99"
    , [9]
    , [0]
    )
  -- immediate mode - lt 8
  , ("3,3,1107,-1,8,3,4,3,99", [9], [0])
  , ("3,3,1107,-1,8,3,4,3,99", [8], [0])
  , ( "3,3,1107,-1,8,3,4,3,99"
    , [7]
    , [1]
    )
  -- position mode - jump if zero
  , ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", [0], [0])
  , ( "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
    , [5]
    , [1]
    )
  -- immediate mode - jump if zero
  , ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", [0], [0])
  , ( "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
    , [5]
    , [1]
    )
  -- check if input is 8
  , ( "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    , [7]
    , [0999]
    )
  , ( "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    , [8]
    , [1000]
    )
  , ( "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    , [9]
    , [1001]
    )
  , ( "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    , []
    , [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
    )
  , ("104,1125899906842624,99"          , [], [1125899906842624])
  , ("1102,34915192,34915192,7,4,7,99,0", [], [1219070632396864])
  ]
