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
import           Data.Maybe                               ( catMaybes )
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
    Nothing        -> show $ (Map.!) tiles pos
    Just positions -> case pos `elem` positions of
      True  -> "O"
      False -> show $ (Map.!) tiles pos

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

day17run :: IO ()
day17run = do
  contents <- readFile "input/day17"
  putStr "Day 17 - Part 1: "
  let output = interactiveComputer contents []
  print $ day17a output
  -- putStr "Day 17 - Part 2: "
  -- print $ day17b contents
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
day17b contents = undefined
