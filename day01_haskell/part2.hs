import System.IO

calc_fuel:: Int -> Int
calc_fuel x = max 0 $ x `quot` 3 - 2

calc_total_fuel :: Int -> Int
calc_total_fuel x =
  let moduleFuel = calc_fuel x
  in case moduleFuel of 
    0 -> moduleFuel
    _ -> moduleFuel + calc_total_fuel moduleFuel


examples :: [(Int,Int)]
examples = [
  (14,2),
  (1969,966),
  (100756,50346)
  ]

check_examples = 
  let 
    (i,o) = unzip examples
    r = map calc_total_fuel i
  in 
    (o == r, zip o r)

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let numbers = map read $ lines contents :: [Int]
  let s = sum $ map calc_total_fuel numbers 
  putStrLn "Required Total Fuel: "
  print s
  hClose handle
  