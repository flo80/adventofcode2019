module AOC2019.Day16
  ( day16a
  , day16b
  , day16run
  )
where

import           Data.Char                                ( digitToInt )
import qualified Data.Vector.Unboxed           as V


day16run :: IO ()
day16run = do
  contents <- readFile "input/day16"
  putStr "Day 16 - Part 1: "
  putStrLn $ day16a contents
  putStr "Day 16 - Part 2: "
  putStrLn $ day16b contents
  putStrLn ""


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
      where pattern = patternFor pos

    patternFor :: Int -> [Int]
    patternFor digit = drop 1 $ concatMap (replicate digit) basePattern

    basePattern :: [Int]
    basePattern = cycle [0, 1, 0, (-1)]


day16b :: String -> String
day16b contents = case (length input - offset) > offset of
  -- only calculating a partial sum from offset on (i.e. pattern all 1)
  -- if remaining input is longer than offset, new pattern would come into play
  True  -> error "implemented calculation will fail"
  False -> concatMap (show) $ take 8 hundredth
 where
  input         = concat $ replicate 10000 $ map digitToInt contents
  offset        = read $ take 7 contents

  -- elements before the offset can be ignored since base pattern is 0
  relevantInput = drop offset input

  -- performance optimization from https://www.reddit.com/r/adventofcode/comments/ebai4g/2019_day_16_solutions/fb6gzyd/
  hundredth     = V.toList $ (!! 100) $ iterate phase $ V.fromList relevantInput
  phase input = V.map lastDigit $ V.scanr1' (+) input


lastDigit :: Int -> Int
lastDigit = (flip $ mod) 10 . abs
