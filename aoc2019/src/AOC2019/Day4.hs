module AOC2019.Day4
  ( day4a
  , day4b
  , day4run
  )
where

import           Data.List                                ( group
                                                          , sort
                                                          )


day4run :: IO ()
day4run = do
  contents <- readFile "input/day4"
  putStr "Day 4 - Part 1: "
  print $ day4a contents
  putStr "Day 4 - Part 2: "
  print $ day4b contents
  putStrLn ""


parseInput :: String -> [Int]
parseInput contents = map read $ lines $ contents

day4a :: String -> Int
day4a contents = length $ filter (check) [start .. end]
 where
  (start : end : _) = parseInput contents
  check pw = check_length pw && check_same1 pw && check_increase pw

day4b :: String -> Int
day4b contents = length $ filter (check) [start .. end]
 where
  (start : end : _) = parseInput contents
  check pw = check_length pw && check_same2 pw && check_increase pw


type Password = [Int]

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: Integral x => [x] -> x
fromDigits = foldl addDigit 0 where addDigit num d = 10 * num + d

-- It is a six-digit number.
check_length :: Int -> Bool
check_length pw = pw >= 100000 && pw <= 999999

-- Two adjacent digits are the same (like 22 in 122345)
check_same1 :: Int -> Bool
check_same1 pw =
  let pw_string = show pw
      groups    = group pw_string
  in  any (\item -> (length item) >= 2) groups

-- Two adjacent digits are the same (like 22 in 122345)
-- the two adjacent matching digits are not part of a larger group of matching digits.
check_same2 :: Int -> Bool
check_same2 pw =
  let pw_string = show pw
      groups    = group pw_string
  in  any (\item -> (length item) == 2) groups

-- Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
check_increase :: Int -> Bool
check_increase pw = let digits = toDigits pw in digits == sort digits
