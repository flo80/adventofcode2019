module AOC2019.Day5
  ( day5a
  , day5b
  , day5run
  )
where

import           AOC2019.IntCodeComputer

day5run :: IO ()
day5run = do
  contents <- readFile "input/day5"
  putStr "Day 5 - Part 1: "
  print $ day5a contents
  putStr "Day 5 - Part 2: "
  print $ day5b contents
  putStrLn ""

day5a :: String -> Int
day5a contents =  last $ interactiveComputer contents [1]

day5b :: String -> Int
day5b contents = head $ interactiveComputer contents [5]
