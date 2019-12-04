import System.IO

calc_fuel:: Int -> Int
calc_fuel x = x `quot` 3 - 2


main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let numbers = map read $ lines contents :: [Int]
  let s = sum $ map calc_fuel numbers 
  putStrLn "Required Fuel: "
  print s
  hClose handle
  