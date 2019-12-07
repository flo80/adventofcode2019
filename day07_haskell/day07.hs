import           IntCodeComputer
import           Data.List                                ( nub
                                                          , permutations
                                                          , foldl
                                                          )
import           Debug.Trace

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  putStr "Part2: "
  print $ part2 $ contents


-- PART 1 
part1 :: String -> Int
part1 contents =
  let combinations =
          filter (\c -> Data.List.nub c == c) $ Data.List.permutations [0 .. 4] :: [ Setting
            ]
      code    = parseProgram contents
      options = runChain code combinations
  in  maximum $ snd $ unzip options

type Setting = [Int]
runChain :: Memory -> [Setting] -> [(Setting, Int)]
runChain code settings = map (\s -> (s, runChain' s code)) settings
 where
  runChain' setting code = foldl (\o s -> execute code [s, o]) 0 setting
  execute code input = head $ output $ runComputer $ newComputer code input

examples1 =
  [ ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", 43210)
  , ( "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    , 54321
    )
  , ( "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    , 65210
    )
  ]

testExamples1 = (map part1 $ map fst examples1) == (map snd examples1)



-- PART 2
part2 :: String -> Int
part2 contents =
  let combinations =
          filter (\c -> Data.List.nub c == c) $ Data.List.permutations [5 .. 9] :: [ Setting
            ]
      code    = parseProgram contents
      options = map (\s -> runSetting code s) combinations
  in  maximum options


runSetting :: Memory -> Setting -> Int
runSetting code setting =
  let computers = map (\s -> newComputer code [s]) setting
  in  runLoop (computers, [0])

-- runs one chain until the final output
runLoop :: ([Computer], [Int]) -> Int
runLoop (computers, input) | (state $ last computers) == Halted = last $ input
                           | otherwise = runLoop $ runLoop' (computers, input)

-- iterates once over the chain (and provides outputs as input for next loop)                           
runLoop' :: ([Computer], [Int]) -> ([Computer], [Int])
runLoop' (computers, input) = foldl run ([], input) computers
 where
  run (updatedComputers, input) c =
    let (uc, o) = execute c input in (updatedComputers ++ [uc], o)

-- sets new input for computer, runs it and gets new state & output back
execute :: Computer -> [Int] -> (Computer, [Int])
execute computer' input =
  let computer = addInput computer' input
      c        = runComputer $ computer
  in  resetOutput c

examples2 =
  [ ( "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    , 139629729
    )
  , ( "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    , 18216
    )
  ]

testExamples2 = (map part2 $ map fst examples2) == (map snd examples2)
