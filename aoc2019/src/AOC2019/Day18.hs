{-# LANGUAGE RecordWildCards #-}

module AOC2019.Day18
  ( day18a
  , day18b
  , day18run
  )
where

import           AOC.Common
import           Data.Foldable                            ( toList )
import           Data.Function                            ( on )
import           Data.List                                ( minimumBy )
import           Data.Map                                 ( Map
                                                          , (!)
                                                          )
import qualified Data.Map                      as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Linear.V2
import           Data.Char                                ( isLower
                                                          , isUpper
                                                          , toLower
                                                          , toUpper
                                                          )

day18run :: IO ()
day18run = do
  contents <- readFile "input/day18"
  putStr "Day 18 - Part 1: "
  print $ day18a contents
  putStr "Day 18 - Part 2: "
  print $ day18b contents
  putStrLn ""

-- Character
data Player = Player {position:: Position, keys :: Set Char, missingKeys :: Set Position} deriving (Show)

-- World 
type Tiles = Map Position Tile
data Tile = Wall | Open | Entrance | Door Char | Key Char deriving (Eq)
instance Show Tile where
  show Wall     = "\x2593"
  show Open     = " "
  show Entrance = "@"
  show (Door x) = [toUpper x]
  show (Key  x) = [x]

parseInput :: String -> Tiles
parseInput = parseTilesFromStringWith mapper
 where
  mapper '#' = Wall
  mapper '.' = Open
  mapper '@' = Entrance
  mapper x | isLower x = Key x
           | isUpper x = Door (toLower x)
  mapper x = error ("unknown symbol " ++ show x)

listKeys :: Tiles -> ([Char], Set Position)
listKeys tiles = (keys, Map.keysSet keyTiles)
 where
  keys     = map getC $ Map.elems $ keyTiles
  keyTiles = Map.filter isKey tiles
  isKey (Key _) = True
  isKey _       = False

getC :: Tile -> Char
getC (Key  c) = c
getC (Door c) = c
getC _        = error "Not a key or door"

isReachable :: Set Char -> Tile -> Bool
isReachable _    Wall     = False
isReachable _    Open     = True
isReachable _    Entrance = True
isReachable _    (Key  _) = True
isReachable keys (Door x) = Set.member x keys


day18a :: String -> Int
day18a contents = length $ snd $ findAllKeys tiles player
 where
  tiles               = parseInput contents
  (keys, missingKeys) = listKeys tiles
  player              = Player (tiles ?! Entrance) Set.empty missingKeys


type Path = [Position]
findAllKeys :: Tiles -> Player -> (Player, Path)
findAllKeys tiles player = minimumBy (compare `on` (length . snd)) options
  where options = findAllNextSteps tiles player []

findAllNextSteps :: Tiles -> Player -> Path -> [(Player, Path)]
findAllNextSteps tiles player@Player {..} path
  | Set.null missingKeys = [(player, path)]
  | otherwise = concatMap (\(pl, pa) -> findAllNextSteps tiles pl pa)
                          possibleContinuations
 where
  possibleContinuations :: [(Player, Path)]
  possibleContinuations =
    map (\x -> (newPlayer x, (init $ wayToDest x) ++ path)) $ reachableKeys

  reachableKeys = filter canReach $ toList missingKeys
  canReach x = length (findShortestPathWith tiles (isReachable keys) position x) > 0

  wayToDest :: Position -> Path
  wayToDest x = findShortestPathWith tiles (isReachable keys) position x

  newPlayer :: Position -> Player
  newPlayer x =
    Player x (Set.insert (getC $ tiles ! x) keys) (Set.delete x missingKeys)


day18b :: String -> Int
day18b = undefined
