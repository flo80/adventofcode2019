module AOC2019.Day11
  ( day11a
  , day11b
  , day11run
  )
where

import           AOC2019.IntCodeComputer
import qualified Data.Map                      as Map
import           Data.Map                                 ( Map )
import           Data.Maybe                               ( fromMaybe )


day11run :: IO ()
day11run = do
  contents <- readFile "input/day11"
  putStr "Day 11 - Part 1: "
  print $ day11a contents
  putStrLn "Day 11 - Part 2: "
  putStrLn $ day11b contents
  putStrLn ""


parseInput :: String -> String
parseInput = id

day11a :: String -> Int
day11a contents =
  length $ Map.keys $ painted $ executeCommand $ newRobot contents Nothing

day11b :: String -> String
day11b contents =
  showTiles $ painted $ executeCommand $ newRobot contents $ Just 1


data Robot = Robot
  { brain :: Computer
  , location :: Position
  , direction :: Direction
  , painted :: Tiles
  } deriving (Show)

type Tiles = Map Position Color
type Position = (Int, Int)
type Color = Int
data Turn = TurnLeft | TurnRight
instance Show Turn where
  show TurnLeft  = "\x27f2"
  show TurnRight = "\x27f3"
gT :: Int -> Turn
gT 0 = TurnLeft
gT 1 = TurnRight
gT _ = error "Turn direction not valid"

data Direction = U | D | L | R deriving (Eq)
instance Show Direction where
  show U = "^"
  show D = "v"
  show L = "<"
  show R = ">"

move :: Turn -> Robot -> Robot
move t = step . turn
 where
  turn :: Robot -> Robot
  turn robot = robot { direction = (direction robot +> t) }
   where
    (+>) :: Direction -> Turn -> Direction
    (+>) U TurnLeft  = L
    (+>) L TurnLeft  = D
    (+>) D TurnLeft  = R
    (+>) R TurnLeft  = U
    (+>) U TurnRight = R
    (+>) R TurnRight = D
    (+>) D TurnRight = L
    (+>) L TurnRight = U

  step :: Robot -> Robot
  step robot = robot { location = newLocation }
   where
    (x, y)      = location robot
    newLocation = case direction robot of
      U -> (x, y + 1)
      D -> (x, y - 1)
      L -> (x - 1, y)
      R -> (x + 1, y)

newRobot :: String -> Maybe Color -> Robot
newRobot code color = Robot { brain     = newComputer code [fromMaybe 0 color]
                            , location  = (0, 0)
                            , direction = U
                            , painted   = Map.empty
                            }

-- Returns last painted color, 0 as default         
getCameraInput :: Robot -> Color
getCameraInput robot =
  fromMaybe 0 $ Map.lookup (location robot) (painted robot)

paintTile :: Color -> Robot -> Robot
paintTile color robot =
  robot { painted = Map.insert (location robot) color (painted robot) }


executeCommand :: Robot -> Robot
executeCommand r@Robot { brain = Computer { state = Halted } } = r
executeCommand r@Robot { brain = Computer { state = Running } } =
  executeCommand $ r { brain = runComputer $ brain r }
executeCommand robot@Robot { brain = c@Computer { state = WaitingForInput } } =
  executeCommand $ (provideInput $ paintMove out newRobot)
 where
  (newComputer, out) = resetOutput $ brain robot
  newRobot           = robot { brain = newComputer }

  paintMove :: [Int] -> Robot -> Robot
  paintMove []                 r = r
  paintMove [color, direction] r = move (gT direction) $ paintTile color r

  provideInput :: Robot -> Robot
  provideInput r = r { brain = newComputer2 }
   where
    color        = getCameraInput r
    newComputer2 = runComputer $ addInput (brain r) [color]


showTiles :: Tiles -> String
showTiles tiles = concat $ map showLine $ reverse [minimum ys .. maximum ys]
 where
  allPositions = Map.keys tiles :: [(Int, Int)]
  (xs, ys)     = unzip allPositions

  showLine :: Int -> String
  showLine y =
    "   " ++ [ showPos (x, y) | x <- [minimum xs .. maximum xs] ] ++ "\n"

  showPos :: Position -> Char
  showPos pos = showTile $ fromMaybe 0 $ Map.lookup pos tiles
   where
    showTile 0 = ' '
    showTile 1 = '\x2588'
    showTile x = error ("undefined color " ++ show x)
