import IntCodeComputer

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  putStr "Part2: "
  print $ part2 $ contents
  
part1 :: String -> Int
part1 contents =
  head $ interactiveComputer contents [1]

part2 :: String -> Int
part2 contents =
  head $ interactiveComputer contents [2]