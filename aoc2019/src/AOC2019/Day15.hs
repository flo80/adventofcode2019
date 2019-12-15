{-# LANGUAGE RecordWildCards #-}

module AOC2019.Day15
  ( day15a
  , day15a_interactive
  , day15b
  , day15run
  )
where

import           Linear.V2
import           System.IO
import           AOC2019.IntCodeComputer
import           System.Console.ANSI
import           Data.Maybe                               ( fromMaybe
                                                          , fromJust
                                                          , catMaybes
                                                          )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.List                                ( length )
import           Debug.Trace

day15run :: IO ()
day15run = do
  contents <- readFile "input/day15"
  putStr "Day 15 - Part 1: "
  print $ day15a contents
  putStr "Day 15 - Part 2: "
  print $ day15b contents
  putStrLn ""


type Position = V2 Int
startPos :: Position
startPos = V2 0 0

type Tile = Map Position TileID

data Direction = N|S|W|E deriving (Show,Eq)
instance Enum Direction where
  toEnum 1 = N
  toEnum 2 = S
  toEnum 3 = W
  toEnum 4 = E
  fromEnum N = 1
  fromEnum S = 2
  fromEnum W = 3
  fromEnum E = 4
calcPos :: Position -> Direction -> Position
calcPos pos d = pos + case d of
  N -> V2 (0) (-1)
  S -> V2 (0) (1)
  W -> V2 (1) (0)
  E -> V2 (-1) (0)

data TileID = Empty | Wall | Oxygen   deriving (Eq)
instance Enum TileID where
  toEnum 0 = Wall
  toEnum 1 = Empty
  toEnum 2 = Oxygen
  fromEnum Wall   = 0
  fromEnum Empty  = 1
  fromEnum Oxygen = 2
instance Show TileID where
  show Empty  = "\x00b7"
  show Wall   = "\x2593"
  show Oxygen = "O"

showWorld :: Tile -> Maybe [Position] -> [IO ()]
showWorld tiles trail =
  clearScreen
    :  (map showT (Map.toList tiles))
    ++ [showStatus tiles]
    ++ case trail of
         Nothing -> []
         Just t  -> (showDistance t) : (map showTrail t)
    ++ [setCursorPosition 50 0]
 where
  showT :: (Position, TileID) -> IO ()
  showT (V2 x y, tid) = do
    setCursorPosition (y + 25) (x + 25)
    case x == 0 && y == 0 of
      True  -> putStr "x"
      False -> putStr $ show tid

  showDistance :: [Position] -> IO ()
  showDistance t = do
    setCursorPosition 1 0
    putStr ("Steps  " ++ (show $ pred $ length t))

  showStatus :: Tile -> IO ()
  showStatus world = do
    setCursorPosition 0 0
    putStr ("Oxygen at " ++ (show $ Map.filter (== Oxygen) tiles))


  showTrail :: Position -> IO ()
  showTrail (V2 x y) = do
    setSGR [SetColor Foreground Vivid Red]
    setCursorPosition (y + 25) (x + 25)
    case x == 0 && y == 0 of
      True  -> putStr "X"
      False -> putStr $ show $ fromJust $ Map.lookup (V2 x y) tiles
    setSGR [Reset]

getWorld contents = (world, walkways, oxygen)
 where
  computer = newComputer contents []
  world    = explore computer
  walkways = Map.keys $ Map.filter (/= Wall) world
  oxygen   = head $ Map.keys $ Map.filter (== Oxygen) world

day15a :: String -> Int
day15a contents = pred $ length distance
 where
  (_, walkways, oxygen) = getWorld contents
  distance              = findShortestPath walkways oxygen startPos

day15b :: String -> Int
day15b contents = maximum $ catMaybes $ Map.elems $ fill walkways
                                                         []
                                                         positions
                                                         oxygen
 where
  (_, walkways, oxygen) = getWorld contents
  positions             = Map.insert oxygen (Just 0) $ Map.fromList $ map
    (\x -> (x, Nothing))
    walkways

  fill
    :: [Position]
    -> [Position]
    -> Map Position (Maybe Int)
    -> Position
    -> Map Position (Maybe Int)
  fill walkways visited distances pos
    | length neighbors == 0 = distances
    | otherwise = foldl (Map.unionWith merge) updDistances newDistances
   where
    neighbors = filter (\k -> not $ k `elem` visited)
      $ filter (\k -> k `elem` nPos) walkways
    nPos        = map (calcPos pos) [N, S, E, W]
    newVisited  = pos : visited
    neiDistance = succ $ fromJust $ fromJust $ Map.lookup pos distances
    updDistances =
      foldl (\a n -> Map.insert n (Just neiDistance) a) distances neighbors
    newDistances = map (fill walkways newVisited updDistances) neighbors

    merge :: Maybe Int -> Maybe Int -> Maybe Int
    merge Nothing  Nothing  = Nothing
    merge Nothing  (Just x) = Just x
    merge (Just x) Nothing  = Just x
    merge (Just x) (Just y) = Just (min x y)


day15a_interactive :: IO ()
day15a_interactive = do
  program <- readFile "input/day15"
  let (world, walkways, oxygen) = getWorld program
  let distance                  = findShortestPath walkways oxygen startPos
  sequence_ $ showWorld world (Just distance)


findShortestPath :: [Position] -> Position -> Position -> [Position]
findShortestPath world destination pos =
  foldl1 (\a x -> if length x < length a then x else a)
    $ findDistance' world [] destination pos
 where
  findDistance'
    :: [Position] -> [Position] -> Position -> Position -> [[Position]]
  findDistance' world visited destination pos
    | pos == destination = [pos : visited]
    | otherwise          = concatMap
      (findDistance' world newVisited destination)
      neighbors
   where
    neighbors =
      filter (\k -> not $ k `elem` visited) $ filter (\k -> k `elem` nPos) world
    nPos       = map (calcPos pos) [N, S, E, W]
    newVisited = pos : visited



explore :: Computer -> Tile
explore computer = foldl
  (\a d -> merge a $ exploreDir startTile computer startPos d)
  Map.empty
  [N, S, E, W]
 where
  startTile = Map.singleton startPos Empty

  explore' :: Tile -> Computer -> Position -> Tile
  explore' tiles computer currentPos = case Map.lookup currentPos tiles of
    (Just _) -> tiles
    Nothing  -> Map.union tiles newTiles
   where
    (nc, o) = resetOutput $ runComputer computer
    tid     = case length o of
      0 -> error "no output"
      1 -> Just $ toEnum $ head o
      _ -> error ("too many outputs" ++ show o)
    newTiles = case tid of
      -- Nothing   -> foldl map (exploreDir tiles nc currentPos) [N, S, E, W]
      Just Wall -> Map.singleton currentPos Wall
      Just t    -> foldl (\a d -> merge a $ exploreDir a nc currentPos d)
                         nt
                         [N, S, E, W]
        where nt = merge tiles $ Map.singleton currentPos t


  exploreDir :: Tile -> Computer -> Position -> Direction -> Tile
  exploreDir tiles computer position dir =
    explore' tiles (addInput computer [fromEnum dir]) (calcPos position dir)

  merge :: Tile -> Tile -> Tile
  merge a b = Map.union a b
