module AOC2019.Day16
  ( day16a
  , day16b
  , day16run
  )
where

import           Data.Char                                ( digitToInt )

day16run :: IO ()
day16run = do
  contents <- readFile "input/day16"
  putStr "Day 16 - Part 1: "
  putStrLn $ day16a contents
  putStr "Day 16 - Part 2: "
  putStrLn $ day16b contents


day16a :: String -> String
day16a contents = concatMap (show) $ take 8 hundredth
 where
  input     = map digitToInt contents
  hundredth = last $ take 101 $ iterate fft input

  fft :: [Int] -> [Int]
  fft input = map calcDigit $ zip [1 ..] input
   where
    calcDigit :: (Int, Int) -> Int
    calcDigit (pos, value) = lastDigit $ sum $ zipWith (*) input pattern
     where
      pattern   = patternFor pos
      lastDigit = (flip $ mod) 10 . abs

    patternFor :: Int -> [Int]
    patternFor digit = drop 1 $ concatMap (replicate digit) basePattern

    basePattern :: [Int]
    basePattern = cycle [0, 1, 0, (-1)]


day16b :: String -> String
day16b contents = case (length input - offset) > offset of
  True  -> error "implemented calculation will fail"
  False -> concatMap (show) $ take 8 hundredth
 where
  input         = concat $ replicate 10000 $ map digitToInt contents
  offset        = read $ take 7 contents

  -- elements before the offset can be ignored since base pattern is 0
  relevantInput = drop offset input
  hundredth     = last $ take 101 $ iterate phase relevantInput

  phase :: [Int] -> [Int]
  phase input = phase' input (sum input)
    where
      phase' [] _ = []
      phase' (x : xs) partialSum =
        (lastDigit partialSum) : phase' xs (partialSum - x)

      lastDigit :: Int -> Int
      lastDigit = (flip $ mod) 10 . abs