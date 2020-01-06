module AOC2019.Day24
  ( day24a
  , day24b
  , day24run
  )
where

import           AOC.Common
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
  print $ day24b contents
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

day24a :: String -> Int
day24a = bioDiversity . findRepetition . parseInput

day24b :: String -> Int
day24b = undefined
