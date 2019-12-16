import           IntCodeComputer
import           Data.List                                ( nub
                                                          , permutations
                                                          , foldl
                                                          )
import           Debug.Trace

main = do
  contents <- readFile "input"
  putStr "Part2: "
  print $ part2 $ contents



-- PART 2
type Setting = [Int]
part2 :: String -> Int
part2 contents =
  let combinations =
          filter (\c -> Data.List.nub c == c) $ Data.List.permutations [5 .. 9] :: [ Setting]
      code    = parseProgram contents
      options = map (\s -> runSetting code s) combinations
  in  maximum options


runSetting :: Memory -> Setting -> Int
runSetting code [a,b,c,d,e] =
  let 
    oa = interactiveComputer code (a:0:oe)
    ob = interactiveComputer code (b:oa)
    oc = interactiveComputer code (c:ob)
    od = interactiveComputer code (d:oc)
    oe = interactiveComputer code (e:od)
  in  last oe



examples2 =
  [ ( "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    , 139629729
    )
  , ( "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    , 18216
    )
  ]

testExamples2 = (map part2 $ map fst examples2) == (map snd examples2)
