module AOC2019.Day2
  ( day2a
  , day2b
  , day2run
  )
where

import           AOC2019.IntCodeComputer
import           Data.List                                ( intersperse )
import           Data.List.Split                          ( splitOn )
import           Data.Sequence                            ( index )
import           Debug.Trace

day2run :: IO ()
day2run = do
  contents <- readFile "input/day2"
  putStr "Day 2 - Part 1: "
  print $ day2a contents
  putStr "Day 2 - Part 2: "
  print $ day2b contents
  putStrLn ""


day2a :: String -> Int
day2a contents = runAndMem computer
  where computer = patchComputer contents (12, 02)

-- This is complicated since revised versions of the IntCodeComputer did not forsee patching memory
patchComputer :: String -> (Int, Int) -> Computer
patchComputer contents (b, c) = newComputer patchedProgram []
 where
  (a : _ : _ : xs) = map read $ splitOn "," contents :: [Int]
  patchedProgram   = drop 1 $ concatMap ((++) ",") $ map show $ a : b : c : xs

runAndMem :: Computer -> Int
runAndMem = (flip $ index) 0 . memory . runComputer

-- Not very fast or efficient but works simple
day2b :: String -> Int
day2b contents = snd $ head $ dropWhile (\(c, _) -> c /= 19690720) results
 where
  options      = [ (n, v) | n <- [0 .. 99], v <- [0 .. 99] ]
  computations = map (runAndMem . patchComputer contents) options
  results      = zip computations $ map (\(a, b) -> a * 100 + b) options
