module AOC2019.Day21
  ( day21a
  , day21a_interactive
  , day21b
  , day21run
  )
where

import           AOC2019.IntCodeComputer
import           Data.List                                ( unfoldr, intercalate )
import           System.Console.ANSI
import           Debug.Trace

day21run :: IO ()
day21run = do
  contents <- readFile "input/day21"
  putStr "Day 21 - Part 1: "
  print $ day21a contents
  -- putStr "Day 21 - Part 2: "
  -- print $ day21b contents
  -- day21a_interactive contents
  putStrLn ""


showOutput :: [Int] -> String
showOutput = concatMap showChar 
  where 
    showChar :: Int -> String
    showChar c
      | c > 255 = show $ c
      | otherwise = [toEnum c]

encodeInput :: String -> [Int]
encodeInput "NOOP" = []
encodeInput x = map fromEnum (x ++ "\n")




day21a :: String -> Int
day21a contents = last $ interactiveComputer contents $ encodeInput program
    where 
      program = unlines
        ["NOT A J"
        ,"NOT B T"
        ,"OR T J"
        ,"NOT C T"
        ,"OR T J"
        ,"AND D J"
        ,"WALK"
        ]

day21a_interactive ::  IO ()
day21a_interactive  = do
  contents <- readFile "input/day21"
  let computer = newComputer contents []
  loop_interactive computer False []

loop_interactive :: Computer -> Bool -> String -> IO ()
loop_interactive c outputPhase program = do
  let (nc, o) = resetOutput $ runComputer c
  case outputPhase of 
    False -> do 
      putStr $ showOutput o
      newIn <- getLine
      let nc2 = addInput nc $ encodeInput newIn
      let op = if newIn == "WALK" then True else False
      loop_interactive nc2 op (program ++ newIn ++ "\n")
    True -> do 
      clearScreen
      printTextColumn program 40 1
      setCursorPosition 0 0
      putStrLn $ showOutput o
      case last o > 255 of
        True -> putStrLn "Success"
        False -> putStrLn "Failed"
    

printTextColumn :: String -> Int -> Int -> IO()
printTextColumn text x y = sequence_ $ map pln $ zip [y..] $ lines text 
      where 
        pln (py, s) = do
          setCursorPosition py x 
          putStr s
    

day21b :: String -> Int
day21b = undefined
