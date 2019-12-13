{-# LANGUAGE RecordWildCards #-}

module AOC2019.Day13
  ( day13a
  , day13b
  , day13b_runInteractive
  , day13run
  )
where

import           System.IO
import           System.Timeout
import           AOC2019.IntCodeComputer
import           System.Console.ANSI
import           Control.Monad
import           Data.Maybe                               ( fromMaybe )
import           Data.List                                ( find )


day13run :: IO ()
day13run = do
  contents <- readFile "input/day13"
  putStr "Day 13 - Part 1: "
  print $ day13a contents
  putStr "Day 13 - Part 2: "
  print $ day13b contents
  putStrLn ""


data Tile = Tile {x:: Int, y::Int, tid :: TileID} | Score Int deriving (Show, Eq)

parseTiles :: [Int] -> [Tile]
parseTiles []                 = []
parseTiles (x : y : t : rest) = tile (x, y, t) : parseTiles rest
 where
  tile (-1, 0, s) = Score s
  tile (x , y, t) = Tile x y (toEnum t)

showTiles :: [Tile] -> [IO ()]
showTiles tiles = map showT tiles
 where
  showT :: Tile -> IO ()
  showT Tile {..} = do
    setCursorPosition y x
    putStr $ show tid
  showT (Score score) = do
    setCursorPosition 0 60
    setSGR [SetColor Foreground Vivid Red]
    putStr $ show score
    setSGR [Reset]

data TileID = Empty | Wall | Block | Paddle | Ball deriving (Eq)
instance Enum TileID where
  toEnum 0 = Empty
  toEnum 1 = Wall
  toEnum 2 = Block
  toEnum 3 = Paddle
  toEnum 4 = Ball
  fromEnum Empty  = 0
  fromEnum Wall   = 1
  fromEnum Block  = 2
  fromEnum Paddle = 3
  fromEnum Ball   = 4
instance Show TileID where
  show Empty  = " "
  show Wall   = "\x2593"
  show Block  = "\x2591"
  show Paddle = "\x2594"
  show Ball   = "\x25EF"

day13a :: String -> Int
day13a contents = length $ filter (\(_, _, t) -> t == 2) $ getTiles output
 where
  output = interactiveComputer contents []
  getTiles :: [Int] -> [(Int, Int, Int)]
  getTiles (x : y : t : rest) = (x, y, t) : getTiles rest
  getTiles []                 = []


day13b :: String -> Int
day13b contents = simulateGame (Nothing, Nothing) computer
 where
  program  = "2" ++ (drop 1 contents) :: String
  computer = newComputer program []

simulateGame :: (Maybe Tile, Maybe Tile) -> Computer -> Int
simulateGame oPaddleBall computer = out
 where
  nc  = runComputer computer
  out = case state nc of

    Halted -> finalScore $ parseTiles $ output nc
     where
      finalScore :: [Tile] -> Int
      finalScore = foldl (+) 0 . map fs
       where
        fs (Score score) = score
        fs _             = 0

    WaitingForInput -> simulateGame paddleBall $ addInput nc2 [i]
     where
      (nc2, o)   = resetOutput nc
      tiles      = parseTiles o
      paddleBall = getPaddleBall oPaddleBall tiles
      i          = parseInput Nothing paddleBall

    _ -> error "missing state"


parseInput :: Maybe Char -> (Maybe Tile, Maybe Tile) -> Int
parseInput (Just 'j') _              = (-1)
parseInput (Just 'l') _              = (1)
parseInput (Just _  ) _              = (0)
parseInput Nothing    (paddle, ball) = signum (bx - px)
 where
  (px, bx) = case paddle of
    Nothing            -> (0, 0)
    Just (Tile px _ _) -> case ball of
      Nothing            -> (px, px)
      Just (Tile bx _ _) -> (px, bx)

getPaddleBall :: (Maybe Tile, Maybe Tile) -> [Tile] -> (Maybe Tile, Maybe Tile)
getPaddleBall (oPaddle, oBall) tiles = (paddle, ball)
 where
  nPaddle     = find (\t -> tid t == Paddle) filterTiles
  nBall       = find (\t -> tid t == Ball) filterTiles
  filterTiles = filter isTile tiles
  isTile (Tile _ _ _) = True
  isTile _            = False

  paddle = case nPaddle of
    Nothing -> oPaddle
    _       -> nPaddle
  ball = case nBall of
    Nothing -> oBall
    _       -> nBall



-- Interactive version

-- speed is waiting time between cycle
speed :: Int
speed = 300000

-- with cheat, computer plays if no input
cheat :: Bool
cheat = False


day13b_runInteractive :: IO ()
day13b_runInteractive = do
  contents <- readFile "input/day13"
  day13b_interactive contents
  s <- getLine
  putStrLn ""

day13b_interactive :: String -> IO ()
day13b_interactive contents = do
  let program  = "2" ++ (drop 1 contents) :: String
  let computer = newComputer program []
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  clearScreen
  hideCursor
  playGame 1 (Nothing, Nothing) computer


playGame :: Int -> (Maybe Tile, Maybe Tile) -> Computer -> IO ()
playGame loopCounter oPaddleBall computer = do
  printStatus "Processing                    "
  printLoopCounter loopCounter
  let nc = runComputer computer

  case state nc of
    Halted -> do
      let o = output nc
      printStatus "Ended                     "
      sequence_ $ showTiles $ parseTiles o
      setCursorPosition 40 0
      showCursor
      putStr "Part 2: "
      putStrLn $ finalScore $ parseTiles o


    WaitingForInput -> do
      let (nc2, o) = resetOutput nc
      let tiles    = parseTiles o
      let paddleBall =
            (case cheat of
              True  -> getPaddleBall oPaddleBall tiles
              False -> (Nothing, Nothing)
            )

      sequence_ (showTiles $ tiles)
      printStatus "Waiting for Input"
      setCursorPosition 40 0
      -- set speed of game with timeout
      c <- timeout speed getChar
      let i = parseInput c paddleBall
      playGame (loopCounter + 1) paddleBall $ addInput nc2 [i]

    _ -> error "missing state"
 where
  finalScore :: [Tile] -> String
  finalScore = foldl (++) "" . map fs
   where
    fs (Score score) = show score
    fs _             = ""

  printStatus :: String -> IO ()
  printStatus status = sequence_ [setCursorPosition 1 60, putStr status]

  printLoopCounter :: Int -> IO ()
  printLoopCounter count =
    sequence_ [setCursorPosition 2 60, putStr $ "loop: " ++ show count]


