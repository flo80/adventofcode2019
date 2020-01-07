module AOC2019.Day21
  ( day21a
  , day21_interactive
  , day21b
  , day21run
  )
where

import           AOC2019.IntCodeComputer
import           Data.List                                ( intercalate )
import           Data.List.Split                          ( splitOn )
import           Data.Char                                ( toUpper )
import           System.Console.ANSI
import           System.Timeout
import           System.IO                                ( stdin
                                                          , hSetEcho
                                                          )
day21run :: IO ()
day21run = do
  contents <- readFile "input/day21"
  putStr "Day 21 - Part 1: "
  print $ day21a contents
  putStr "Day 21 - Part 2: "
  print $ day21b contents
  putStrLn ""


showOutput :: [Int] -> String
showOutput = concatMap showChar
 where
  showChar :: Int -> String
  showChar c | c > 255   = show $ c
             | otherwise = [toEnum c]

encodeInput :: String -> [Int]
encodeInput "NOOP" = []
encodeInput x      = map fromEnum (x ++ "\n")



day21a :: String -> Int
day21a contents = last $ interactiveComputer contents $ encodeInput program
 where
  program = unlines
    ["NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "WALK"]

day21b :: String -> Int
day21b contents = last $ interactiveComputer contents $ encodeInput program
 where
  program = unlines
    [ "NOT A J"
    , "NOT C T"
    , "AND H T"
    , "OR T J"
    , "NOT B T"
    , "AND A T"
    , "AND C T"
    , "OR T J"
    , "AND D J"
    , "RUN"
    ]



day21_interactive :: IO ()
day21_interactive = do
  contents <- readFile "input/day21"
  let computer = newComputer contents []
  clearScreen
  setCursorPosition 0 0
  putStrLn "Springscript programming"
  printHelp
  setCursorPosition 2 0
  loop_interactive computer False []

loop_interactive :: Computer -> Bool -> String -> IO ()
loop_interactive c outputPhase program = do
  let (nc, o) = resetOutput $ runComputer c
  case outputPhase of
    False -> do
      putStr $ showOutput o
      showCursor
      newIn' <- getLine
      let newIn = map toUpper newIn'
      let nc2   = addInput nc $ encodeInput newIn
      let op = if (newIn == "WALK" || newIn == "RUN") then True else False
      loop_interactive nc2 op (program ++ newIn ++ "\n")

    True -> do
      clearScreen
      hideCursor
      hSetEcho stdin False
      printTextColumn 40 1 program
      setCursorPosition 0 0
      case last o > 255 of
        True  -> putStrLn $ showOutput o
        False -> renderOutputMovie $ showOutput o

      setCursorPosition ((length $ lines program) + 2) 40
      case last o > 255 of
        True  -> putStrLn "Success"
        False -> putStrLn "Failed"
      setCursorPosition 10 0
      putStrLn "Press q to end or any other key to start again"
      r <- getChar
      hSetEcho stdin True
      case r == 'q' of
        True -> do
          showCursor
          return ()
        False -> day21_interactive

renderOutputMovie :: String -> IO ()
renderOutputMovie movie = do
  let [intro, scenes] = splitOn ":" movie
  let [walk, across]  = splitOn "\n\n" intro
  let indivScenes     = splitOn "\n\n" scenes
  putStrLn walk
  sequence_ $ map renderScene indivScenes
  setCursorPosition 8 0
  putStrLn across

 where
  renderScene scene = do
    setCursorPosition 3 0
    putStrLn scene
    _ <- timeout 300000 getChar
    putStr ""

printTextColumn :: Int -> Int -> String -> IO ()
printTextColumn x y text = sequence_ $ map pln $ zip [y ..] $ lines text
 where
  pln (py, s) = do
    setCursorPosition py x
    putStr s

printHelp :: IO ()
printHelp = printTextColumn 40 0 $ unlines
  [ "Springbook instructions"
  , " AND X Y sets Y to true if both X and Y are true; otherwise, it sets Y to false"
  , " OR X Y sets Y to true if at least one of X or Y is true; otherwise, it sets Y to false"
  , " NOT X Y sets Y to true if X is false; otherwise, it sets Y to false"
  , " WALK starts a program in normal mode"
  , " RUN starts a program with increased sensor range"
  , ""
  , "Input registers corresponding to positions in front of robot"
  , " Normal mode: A,B,C,D"
  , " Increase sensor range: A,B,C,D,E,F,G,H,I"
  , " Registers are true if there is ground, false if there is a hole"
  , ""
  , "Output registers (can also be used as input)"
  , " T - temporary"
  , " J - jump register, if true at end of program, robot will try to jump"
  ]
