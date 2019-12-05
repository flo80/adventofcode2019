import Data.List (group, sort)

type Password = [Int]

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: Integral x => [x] -> x
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d


-- It is a six-digit number.
check_length :: Int -> Bool
check_length pw = 
  pw >= 100000 && pw <= 999999

-- Two adjacent digits are the same (like 22 in 122345)
check_same :: Int -> Bool
check_same pw =
  let 
    pw_string = show pw
    groups = group pw_string
  in
    any (\item -> (length item) >=2) groups

  -- Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
check_increase :: Int -> Bool
check_increase pw =
  let 
    digits = toDigits pw
  in 
    digits == sort digits

is_valid :: Int -> Bool
is_valid pw = 
  check_length pw &&
  check_same pw &&
  check_increase pw

getNumberOfValids :: Int -> Int -> Int
getNumberOfValids start end =
  let
    numbers = [start..end]
    valids = filter (is_valid)  numbers
  in 
    length valids

main = do
  contents <- readFile "input" -- two lines, start \n end
  let i = map read $ lines $ contents
  let start = head i
  let end = last i
  putStrLn $ "checking from " ++ show start ++ " to " ++ show end
  putStr "Result: "
  print $ getNumberOfValids start end