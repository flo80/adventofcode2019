-- part 2 is slow, could optimize by using array like in https://github.com/Noble-Mushtak/Advent-of-Code/blob/master/2019/day24p2.hs
module AOC2019.Day24
  ( day24a
  , day24b
  , day24run
  )
where

import           AOC.Common
import           Data.Maybe                               ( fromMaybe )
import           Data.List                                ( (\\)
                                                          , foldl'
                                                          )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Linear.V2

day24run :: IO ()
day24run = do
  contents <- readFile "input/day24"
  putStr "Day 24 - Part 1: "
  print $ day24a contents
  putStr "Day 24 - Part 2: "
  print $ day24b 200 contents
  putStrLn ""


data Tile = Dead | Alive deriving (Eq, Ord)
instance Show Tile where
  show Dead  = "."
  show Alive = "#"
instance Enum Tile where
  toEnum 46 = Dead
  toEnum 35 = Alive
  fromEnum Dead  = 46
  fromEnum Alive = 35
  succ Dead  = Alive
  succ Alive = Dead
  pred = succ

parseInput :: String -> Map Position Tile
parseInput = parseTilesFromString


day24a :: String -> Int
day24a = bioDiversity . findRepetition . parseInput


nextState :: Map Position Tile -> Map Position Tile
nextState tiles = Map.mapWithKey evolution tiles
 where
  evolution position state = case state of
    Alive -> if nbs == 1 then Alive else Dead
    Dead  -> if nbs == 1 || nbs == 2 then Alive else Dead
   where
    adj = map (+ position) adjacentPositions
    nbs =
      Map.size $ Map.filterWithKey (\k v -> k `elem` adj && v == Alive) tiles

findRepetition :: Map Position Tile -> Map Position Tile
findRepetition tiles = findRepetition' Set.empty tiles
 where
  findRepetition' previous state = case Set.member state previous of
    True  -> state
    False -> findRepetition' (Set.insert state previous) $ nextState state

bioDiversity :: Map Position Tile -> Int
bioDiversity tiles = sum $ map score liveTiles
 where
  liveTiles = Map.keys $ Map.filter (== Alive) tiles
  score (V2 x y) = 2 ^ (x + 5 * y)


day24b :: Int -> String -> Int
day24b iterations contents = count $ iterate $ addLevel $ parseInput contents
 where
  addLevel :: Map Position Tile -> Map (Int, Position) Tile
  addLevel = Map.fromList . map (\(pos, v) -> ((0, pos), v)) . Map.toList

  iterate tiles = foldl' (\t _ -> nextStateMulti t) tiles [1 .. iterations]
  count = Map.size . Map.filter (== Alive)


nextStateMulti :: Map (Int, Position) Tile -> Map (Int, Position) Tile
nextStateMulti tiles = Map.mapWithKey evolution relevantTiles
 where
  aliveTiles = Map.filter (== Alive) tiles
  allNeighbors =
    Map.fromList $ map (\p -> (p, Dead)) $ concatMap adjacent $ Map.keys
      aliveTiles
  -- combine neighbors and existing active tiles
  relevantTiles = Map.union aliveTiles allNeighbors

  evolution (level, position) state = case state of
    Alive -> if nbs == 1 then Alive else Dead
    Dead  -> if nbs == 1 || nbs == 2 then Alive else Dead
   where
    adj = adjacent (level, position)
    nbs = Map.size
      $ Map.filterWithKey (\k v -> k `elem` adj && v == Alive) relevantTiles

  adjacent :: (Int, Position) -> [(Int, Position)]
  adjacent (_, V2 2 2) = error "requesting neighbor for middle"
  adjacent p@(level, V2 2 1) =
    direct p ++ map (\x -> (level + 1, V2 x 0)) [0 .. 4]
  adjacent p@(level, V2 2 3) =
    direct p ++ map (\x -> (level + 1, V2 x 4)) [0 .. 4]
  adjacent p@(level, V2 1 2) =
    direct p ++ map (\y -> (level + 1, V2 0 y)) [0 .. 4]
  adjacent p@(level, V2 3 2) =
    direct p ++ map (\y -> (level + 1, V2 4 y)) [0 .. 4]
  adjacent p@(level, position) = direct p

  direct (level, position) =
    map adjustLevel
      $  (map (\x -> (level, x + position)) adjacentPositions)
      \\ [(level, V2 2 2)]
   where
      -- outer borders
    adjustLevel :: (Int, Position) -> (Int, Position)
    adjustLevel (level, V2 (-1) y) = (level - 1, V2 1 2) -- left
    adjustLevel (level, V2 5 y   ) = (level - 1, V2 3 2) -- right
    adjustLevel (level, V2 x (-1)) = (level - 1, V2 2 1) -- top
    adjustLevel (level, V2 x 5   ) = (level - 1, V2 2 3) -- bottom
    adjustLevel p                  = p


printAll :: Map (Int, Position) Tile -> String
printAll tiles = concatMap
  (\l -> "Level " ++ show l ++ "\n" ++ printLevel tiles l ++ "\n\n")
  [minLevel .. maxLevel]
 where
  (minLevel, maxLevel) =
    foldl (\(minL, maxL) (l, p) -> (min minL l, max maxL l)) (0, 0)
      $ Map.keys tiles

  printLevel tiles level = concatMap row [0 .. 4]
   where
    row y = (concatMap (\x -> get x y) [0 .. 4]) ++ "\n"
    get 2 2 = "?"
    get x y = show $ fromMaybe Dead $ Map.lookup (level, V2 x y) tiles
