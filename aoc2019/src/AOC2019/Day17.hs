module AOC2019.Day17
  ( day17a
  , day17b
  , day17run
  )
where

import           Linear.V2
import           Data.List                                ( groupBy
                                                          , concat
                                                          , sortBy
                                                          , sort
                                                          , nub
                                                          , group
                                                          )
import           Data.List.Split                          ( splitWhen )
import           Data.Maybe                               ( catMaybes
                                                          , fromMaybe
                                                          )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           AOC2019.IntCodeComputer

type Tiles = Map Position TileType

showTiles :: Tiles -> Maybe [Position] -> String
showTiles tiles intersections = concatMap line [0 .. maxY]
 where
  (V2 maxX maxY) = maximum $ Map.keys tiles
  line y = (concat $ map gt [ (V2 x y) | x <- [0 .. maxX] ]) ++ "\n"
  gt pos = case intersections of
    Nothing        -> fromMaybe "?" $ fmap show $ Map.lookup pos tiles
    Just positions -> case pos `elem` positions of
      True  -> "O"
      False -> fromMaybe "?" $ fmap show $ Map.lookup pos tiles

type Position = V2 Int

data TileType = Scaffold | Space | Robot Direction deriving (Eq)
instance Show TileType where
  show Scaffold    = "#"
  show Space       = "."
  show (Robot dir) = show dir
instance Enum TileType where
  fromEnum Scaffold    = 35
  fromEnum Space       = 46
  fromEnum (Robot dir) = fromEnum dir
  toEnum 35 = Scaffold
  toEnum 46 = Space
  toEnum x  = Robot $ toEnum x

data Direction = U | D | L | R deriving (Eq )
instance Show Direction where
  show U = "^"
  show D = "v"
  show L = "<"
  show R = ">"
instance Enum Direction where
  fromEnum U = 94
  fromEnum D = 118
  fromEnum L = 60
  fromEnum R = 62
  toEnum 94  = U
  toEnum 118 = D
  toEnum 60  = L
  toEnum 62  = R
  succ U = R
  succ R = D
  succ D = L
  succ L = U
  pred U = L
  pred L = D
  pred D = R
  pred R = U

data Rob = Rob Position Direction deriving (Show)
data Move = F | TL | TR deriving (Show)
move :: Move -> Rob -> Rob
move TL (Rob p        x) = Rob p (pred x)
move TR (Rob p        x) = Rob p (succ x)
move F  (Rob (V2 x y) U) = Rob (V2 (x) (y - 1)) U
move F  (Rob (V2 x y) D) = Rob (V2 (x) (y + 1)) D
move F  (Rob (V2 x y) L) = Rob (V2 (x - 1) (y)) L
move F  (Rob (V2 x y) R) = Rob (V2 (x + 1) (y)) R
pos :: Rob -> Position
pos (Rob pos dir) = pos
dir :: Rob -> Direction
dir (Rob pos dir) = dir

canMove :: Tiles -> Rob -> Move -> Bool
canMove tiles rob t = case Map.lookup nextPos tiles of
  Nothing        -> False
  Just Space     -> False
  Just Scaffold  -> True
  Just (Robot _) -> True
 where
  nextPos = pos $ case t of
    F -> move F rob
    x -> move F $ move x rob


day17run :: IO ()
day17run = do
  p1 <- readFile "input/day17"
  putStr "Day 17 - Part 1: "
  let o1 = interactiveComputer p1 []
  print $ day17a o1

  let p2 = "2" ++ drop 1 p1
  putStr "Day 17 - Part 2: "
  print $ day17b p2
  putStrLn ""



day17a :: [Int] -> Int
day17a output = sum $ map getAlignment intersections
 where
  intersections = findIntersections $ parseOutput $ output
  getAlignment (V2 x y) = x * y

parseOutput :: [Int] -> Tiles
parseOutput output = Map.fromList $ map
  (\(x, y, v) -> ((V2 x y), v))
  [ (x, y, v) | (y, row) <- (zip [0 ..] lines), (x, v) <- (zip [0 ..] row) ]
 where
  lines :: [[TileType]]
  lines = map (map toEnum) $ splitWhen (== 10) output

findIntersections :: Tiles -> [Position]
findIntersections tiles = nub $ filter hasEnoughNeighbors relevantPositions
 where
  relevantTiles = Map.filter isWalkable tiles
  hasEnoughNeighbors pos = (>= 3) $ length $ neighbors relevantTiles pos

  relevantPositions :: [Position]
  relevantPositions = Map.keys relevantTiles
  isWalkable Scaffold  = True
  isWalkable (Robot _) = True
  isWalkable Space     = False

  neighbors :: Tiles -> Position -> [Bool]
  neighbors ts pos = filter (== True) $ map (\x -> Map.member x ts) $ nPos pos
  nPos pos = map (+ pos) [V2 (-1) (0), V2 (1) (0), V2 (0) (1), V2 (0) (-1)]



day17b :: String -> Int
day17b program = last $ output $ runComputer computer
 where
  computer = newComputer program input
  input    = map fromEnum (m ++ a ++ b ++ c ++ "n\n")
  m        = "A,B,B,C,B,C,B,C,A,A\n"
  a        = "L,6,R,8,L,4,R,8,L,12\n"
  b        = "L,12,R,10,L,4\n"
  c        = "L,12,L,6,L,4,L,4\n"


findPath :: Tiles -> Rob -> [Move]
findPath tiles rob
  | canMove tiles rob F == True  = F : (findPath tiles $ move F rob)
  | canMove tiles rob TL == True = TL : (findPath tiles $ move TL rob)
  | canMove tiles rob TR == True = TR : (findPath tiles $ move TR rob)
  | otherwise                    = []

