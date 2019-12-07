import           IntCodeComputer
import           Data.List                                ( nub
                                                          , permutations
                                                          )

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
  execute code input = head $ getOutput $ runComputer $ newComputer code input

examples1 =
  [ ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", 43210)
  , ( "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    , 54321
    )
  , ( "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    , 65210
    )
  ]

testExample1 = (map part1 $ map fst examples1) == (map snd examples1)
