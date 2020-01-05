module AOC2019.Day7
  ( day7a
  , day7b
  , day7run
  )
where

import           AOC2019.IntCodeComputer
import           Data.List                                ( nub
                                                          , permutations
                                                          , foldl
                                                          )

day7run :: IO ()
day7run = do
  contents <- readFile "input/day7"
  putStr "Day 7 - Part 1: "
  print $ day7a contents
  putStr "Day 7 - Part 2: "
  print $ day7b contents
  putStrLn ""

day7a :: String -> Int
day7a contents =
  let combinations =
          filter (\c -> Data.List.nub c == c) $ Data.List.permutations [0 .. 4] :: [ Setting
            ]
      options = runChain contents combinations
  in  maximum $ snd $ unzip options

type Setting = [Int]
runChain :: String -> [Setting] -> [(Setting, Int)]
runChain code settings = map (\s -> (s, runChain' s code)) settings
 where
  runChain' setting code = foldl (\o s -> execute code [s, o]) 0 setting
  execute code input = head $ output $ runComputer $ newComputer code input


day7b :: String -> Int
day7b contents =
  let combinations =
          filter (\c -> Data.List.nub c == c) $ Data.List.permutations [5 .. 9] :: [ Setting
            ]
      options = map (\s -> runSetting contents s) combinations
  in  maximum options


runSetting :: String -> Setting -> Int
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
