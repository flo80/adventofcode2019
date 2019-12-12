module AOC2019.Day1
  ( day1a
  , day1b
  , day1run
  )
where


day1run :: IO ()
day1run = do
  contents <- readFile "input/day1"
  putStr "Day 1 - Part 1: "
  print $ day1a contents
  putStr "Day 1 - Part 2: "
  print $ day1b contents
  putStrLn ""


day1a :: String -> Int
day1a = sum . map (calcFuelModule . read) . lines

day1b :: String -> Int
day1b = sum . map (calcFuelTotal . read) . lines


calcFuelModule :: Int -> Int
calcFuelModule x = maximum [x `quot` 3 - 2,0]

calcFuelTotal :: Int -> Int
calcFuelTotal x =
  let moduleFuel = calcFuelModule x
  in  case moduleFuel of
        0 -> moduleFuel
        _ -> moduleFuel + calcFuelTotal moduleFuel

