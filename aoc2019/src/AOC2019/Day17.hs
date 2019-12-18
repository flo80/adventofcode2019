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
                                                          , intercalate
                                                          , inits
                                                          )
import           Data.List.Split                          ( splitWhen )
import           Data.Maybe                               ( catMaybes
                                                          , fromMaybe
                                                          )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           AOC2019.IntCodeComputer
import           Control.Monad

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
data Move = F | TL | TR
instance Show Move where
  show F  = "1"
  show TL = "L"
  show TR = "R"

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
  contents <- readFile "input/day17"
  putStr "Day 17 - Part 1: "
  let o1 = interactiveComputer contents []
  print $ day17a o1
  putStr "Day 17 - Part 2: "
  print $ day17b contents
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
day17b = head . day17b_all

day17b_all :: String -> [Int]
day17b_all program = map (last . output . runComputer . newComputer program2)
                         options
 where
  program2           = "2" ++ drop 1 program
  tiles              = parseOutput $ interactiveComputer program []

  -- find robot
  rob                = Rob rpos rdir
  (rpos, Robot rdir) = head $ Map.toList $ Map.filter isRobot tiles
  isRobot (Robot _) = True
  isRobot _         = False

  -- get path and all shortenings
  path    = encodePath $ findPath tiles rob
  options = map (map fromEnum) $ map (formatPathForInput) $ shortenPath path


findPath :: Tiles -> Rob -> [Move]
findPath tiles rob
  | canMove tiles rob F == True  = F : (findPath tiles $ move F rob)
  | canMove tiles rob TL == True = TL : (findPath tiles $ move TL rob)
  | canMove tiles rob TR == True = TR : (findPath tiles $ move TR rob)
  | otherwise                    = []

encodePath :: [Move] -> [String]
encodePath = concat . map replace . group . map show
 where
  replace :: [String] -> [String]
  replace x | length x == 1 = x
            | otherwise     = [show $ length x]


-- inspired by https://github.com/jan-g/advent2019/blob/master/src/Day17.hs#L299            
shortenPath :: [String] -> [([String], [String], [String], [String])]
shortenPath path =
  -- m cannot be longer than 20 characters when encoded
  (filter (\(a, b, c, m) -> expL m <= 20))
    -- in m, only a,b,c, can appear
    $ filter (\(a, b, c, m) -> filterM m)
    -- get all possible combinations
    $ [ (a, b, c, m)
      | a <- candA
      , b <- (candB a)
      , c <- (candC a b)
      , m <- (candM a b c)
      ]
 where
  -- get all candidates for a (i.e. all possible substrings of path which are shorter than 20 when encoded)
  candA :: [[String]]
  candA = filter (\x -> expL x <= 20) $ tail $ inits path

  -- get all candidates for b, as for a but a is not allowed to appear in path
  candB :: [String] -> [[String]]
  candB a =
    filter (not . null)
      $ filter (\x -> expL x <= 20)
      $ filterOut "A"
      $ tail
      $ inits
      $ r'a a

  candC :: [String] -> [String] -> [[String]]
  candC a b =
    filter (not . null)
      $ filter (\x -> expL x <= 20)
      $ filterOut "B"
      $ filterOut "A"
      $ tail
      $ inits
      $ r'b a b

  -- m is the leftover main
  candM :: [String] -> [String] -> [String] -> [[String]]
  candM a b c = [r'c a b c]

  -- remainder of path after taking out a / b / c
  r'a a = replacePath path a "A"
  r'b a b = replacePath (r'a a) b "B"
  r'c a b c = replacePath (r'b a b) c "C"

  filterM m = all (\x -> x == "A" || x == "B" || x == "C") m == True
  filterOut x = map (takeWhile (/= x)) . map (dropWhile (== x))
  expL = length . intercalate ","


replacePath :: [String] -> [String] -> String -> [String]
replacePath [] repl code = []
-- when the beginning of the current path is equal to the replacement, replace it with code
replacePath path repl code | take lr path == repl =
  code : replacePath (drop lr path) repl code
  where lr = length repl
replacePath (x : xs) repl code = x : replacePath xs repl code


formatPathForInput :: ([String], [String], [String], [String]) -> String
formatPathForInput (a, b, c, m) = input
 where
  input = m' ++ a' ++ b' ++ c' ++ "n\n"
  a'    = intercalate "," a ++ "\n"
  b'    = intercalate "," b ++ "\n"
  c'    = intercalate "," c ++ "\n"
  m'    = intercalate "," m ++ "\n"
