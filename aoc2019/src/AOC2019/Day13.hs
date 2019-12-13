module AOC2019.Day13
  ( day13a
  , day13b
  , day13run
  )
where

import           AOC2019.IntCodeComputer

day13run :: IO ()
day13run = do
  contents <- readFile "input/day13"
  putStr "Day 13 - Part 1: "
  print $ day13a contents
  putStr "Day 13 - Part 2: "
  print $ day13b contents
  putStrLn ""


day13a :: String -> Int
day13a contents = length $ filter (\(_, _, t) -> t == 2) $ getTiles output
 where
  output = interactiveComputer contents []
  getTiles :: [Int] -> [(Int, Int, Int)]
  getTiles (x : y : t : rest) = (x, y, t) : getTiles rest
  getTiles []                 = []

day13b :: String -> Int
day13b = undefined
