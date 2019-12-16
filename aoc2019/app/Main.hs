module Main where

import           System.IO                                ( stdin
                                                          , hReady
                                                          , hSetEcho
                                                          , hSetBuffering
                                                          , BufferMode
                                                            ( NoBuffering
                                                            , LineBuffering
                                                            )
                                                          )
import           System.Console.ANSI
import           Control.Monad                            ( when )
import           Data.Maybe                               ( fromMaybe )

import           AOC2019.Day1
import           AOC2019.Day3
import           AOC2019.Day12
import           AOC2019.Day13
import           AOC2019.Day14
import           AOC2019.Day15
import           AOC2019.Day16


options :: [(String, IO ())]
options =
  [ ("Exit\n", return ())
  , ("All"   , runAll)
  , ("Day 1" , day1run)
  , ("Day 3" , day3run)
  , ("Day 12", day12run)
  , ("Day 12", day12run)
  , ("Day 13", day13run)
  , ("Day 14", day14run)
  , ("Day 15", day15run)
  , ("Day 16", day16run)
  ]

main :: IO ()
main = do
  menu 0

runAll :: IO ()
runAll = sequence_ $ map snd $ drop 2 options

menu :: Int -> IO ()
menu selectedIndex = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor
  printOptions options selectedIndex

  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> menu $ max 0 (selectedIndex - 1)
      "\ESC[B" -> menu $ min (length options - 1) (selectedIndex + 1)
      "\n"     -> execute $ snd $ options !! selectedIndex
      "q"      -> return ()
      _        -> menu selectedIndex

execute :: IO () -> IO ()
execute fct = do
  printHeader
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  showCursor
  fct

printHeader :: IO ()
printHeader = do
  setCursorPosition 0 0
  clearScreen
  putStrLn "==================="
  putStrLn "Advent of Code 2019"
  putStrLn "===================\n"

printOptions :: [(String, IO ())] -> Int -> IO ()
printOptions options idx = do
  printHeader
  sequence_ $ map (printOption idx) $ zip [0 ..] $ map fst options


 where
  printOption i (nr, text) = do
    when (nr == i) $ setSGR [SetSwapForegroundBackground True]
    putStrLn text
    setSGR [Reset]

-- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)
