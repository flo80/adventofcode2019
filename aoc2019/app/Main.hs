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
import           Data.Maybe                               ( fromMaybe
                                                          , fromJust
                                                          , catMaybes
                                                          , isJust
                                                          )
import           Data.List                                ( find
                                                          , elemIndex
                                                          )

import           AOC2019.Day1
import           AOC2019.Day3
import           AOC2019.Day12
import           AOC2019.Day13
import           AOC2019.Day14
import           AOC2019.Day15
import           AOC2019.Day16

data Option = Option String (Maybe (IO ())) 
instance Show Option where
  show (Option title _ ) = show title
instance Eq Option where
  (==) (Option a _ ) (Option b _ ) = a == b


menuItems :: [(Option,Bool)]
menuItems =
  --       Title                             Function to call             Include in runAll
  [ (Option "All\n"                          (Just runAll)                ,False)
  , (Option "Day 1"                          (Just day1run)               ,True)
  , (Option "Day 2"                          (Just day2run)               ,True)
  , (Option "Day 3"                          (Just day3run)               ,True)
  , (Option "Day 4"                          (Nothing)                    ,True)
  , (Option "Day 5"                          (Nothing)                    ,True)
  , (Option "Day 6"                          (Nothing)                    ,True)
  , (Option "Day 7"                          (Nothing)                    ,True)
  , (Option "Day 8"                          (Nothing)                    ,True)
  , (Option "Day 9"                          (Nothing)                    ,True)
  , (Option "Day 10"                         (Nothing)                    ,True)
  , (Option "Day 11"                         (Nothing)                    ,True)
  , (Option "Day 12"                         (Just day12run)              ,True)
  , (Option "Day 13"                         (Just day13run)              ,True)
  , (Option "       - Part 2 interactive"    (Just day13b_runInteractive) ,False)
  , (Option "Day 14"                         (Just day14run)              ,True)
  , (Option "Day 15"                         (Just day15run)              ,True)
  , (Option "       - Part 1 visualization"  (Just day15a_interactive)    ,False)
  , (Option "Day 16"                         (Just day16run)              ,True)
  ]

options = map fst menuItems

main :: IO ()
main = do
  menu $ head options

runAll :: IO ()
runAll = sequence_ $ catMaybes $ map (\((Option t f),_) -> f) $ filter (\(o,i) -> i == True) menuItems

menu :: Option -> IO ()
menu selectedOption = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor
  setSGR [SetColor Background Dull Black]
  printOptions options selectedOption

  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> menu $ option (-1)
      "\ESC[B" -> menu $ option (1)
      "\n"     -> execute $ selectedOption
      "q"      -> exit
      _        -> menu selectedOption

 where
  option i = possibleOptions !! mod (i + idxSelOption) (length possibleOptions)
  idxSelOption    = fromJust $ elemIndex selectedOption possibleOptions
  possibleOptions = filter (\(Option t f) -> isJust f) options

exit :: IO ()
exit = do
  setSGR [Reset]
  printHeader
  showCursor
  return ()

execute :: Option -> IO ()
execute o@(Option t Nothing   ) = menu o
execute o@(Option t (Just fct)) = do
  printHeader
  -- hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  showCursor
  fct
  putStrLn "Press any key to return to menu"
  _ <- getKey
  menu o

printHeader :: IO ()
printHeader = do
  setCursorPosition 2 0
  clearScreen
  putStrLn "==================="
  putStrLn "Advent of Code 2019"
  putStrLn "===================\n"


printOptions :: [Option] -> Option -> IO ()
printOptions options idx = do
  printHeader
  putStrLn "Press q to quit\n"
  sequence_ $ map (printOption idx) options
 where
  printOption selected option@(Option t fct) = do
    case fct of
      Nothing -> setSGR [SetColor Foreground Vivid Black]
      Just _  -> setSGR [SetColor Foreground Vivid White]
    when (selected == option) $ setSGR [SetSwapForegroundBackground True]
    putStrLn t
    setSGR [SetSwapForegroundBackground False]

-- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)
