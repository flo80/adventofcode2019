module AOC2019.Day19
  ( day19a
  , day19b
  , day19run
  )
where

import           AOC2019.IntCodeComputer

day19run :: IO ()
day19run = do
  contents <- readFile "input/day19"
  putStr "Day 19 - Part 1: "
  print $ day19a contents
  putStr "Day 19 - Part 2: "
  print $ day19b contents
  putStrLn ""


parseInput :: String -> String
parseInput = id

day19a :: String -> Int
day19a contents = sum $ concatMap
  (\(a, b) -> interactiveComputer contents [a, b])
  [ (a, b) | a <- [0 .. 49], b <- [0 .. 49] ]

day19b :: String -> Int
day19b = undefined
