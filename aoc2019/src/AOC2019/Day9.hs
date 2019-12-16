module AOC2019.Day9
  ( day9a
  , day9b
  , day9run
  )
where

import           AOC2019.IntCodeComputer

day9run :: IO ()
day9run = do
  contents <- readFile "input/day9"
  putStr "Day 9 - Part 1: "
  print $ day9a contents
  putStr "Day 9 - Part 2: "
  print $ day9b contents
  putStrLn ""


parseInput :: String -> String
parseInput = id

day9a :: String -> Int
day9a = head . (flip $ interactiveComputer) [1]

day9b :: String -> Int
day9b = head .(flip $ interactiveComputer) [2]
