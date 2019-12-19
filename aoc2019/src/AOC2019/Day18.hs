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

isEntrance Entrance = True
isEntrance _        = False
isKey (Key _) = True
isKey _       = False
isDoor (Door _) = True
isDoor _        = False
isWalkable Wall = False
isWalkable _    = True


day18a :: String -> Int
day18a contents = snd $ findAllKeys tiles player
 where
  tiles               = parseInput contents
  (keys, missingKeys) = listKeys tiles
  player              = Player (tiles ?! Entrance) Set.empty missingKeys


type Path = [Position]
findAllKeys :: Tiles -> Player -> (Player, Int)
findAllKeys tiles player = minimumBy (compare `on` snd) options
 where
  options = findAllNextSteps tiles allD player 0
  allD = allDistances tiles

findAllNextSteps
  :: Tiles
  -> Map (Position, Position) (Int, [Char], [Char])
  -> Player
  -> Int
  -> [(Player, Int)]
findAllNextSteps tiles allD player@Player {..} prevDist
  | Set.null missingKeys = [(player, prevDist)]
  | Map.null possibleNextHops = error "no possible hops"
  | otherwise = concatMap (\(pl, pa) -> findAllNextSteps tiles allD pl pa)
                          possibleContinuations
 where
  possibleContinuations :: [(Player, Int)]
  possibleContinuations =
    map createContinuations $ Map.toList $ possibleNextHops

  createContinuations
    :: ((Position, Position), (Int, [Char], [Char])) -> (Player, Int)
  createContinuations ((_, x), (dist, _, getK)) =
    (newPlayer x getK, prevDist + dist)

  possibleNextHops :: Map (Position, Position) (Int, [Char], [Char])
  possibleNextHops = Map.filterWithKey
    (\(a, b) (_, reqK, _) ->
      a
        == position
        && Set.member b missingKeys
        && all (\k -> Set.member k keys) reqK
    )
    allD

  newPlayer :: Position -> [Char] -> Player
  newPlayer x newKeys = Player x ks (Set.delete x missingKeys)
    where ks = Set.union keys $ Set.fromList newKeys

-- calculate  between a,b, distance, doors (i.e. keys required), keys on the way
allDistances :: Tiles -> Map (Position, Position) (Int, [Char], [Char])
allDistances tiles = Map.fromList $ map
  (\(a, b) -> ((a, b), (getDistance a b, getDoors a b, getKeys a b)))
  allPairs
 where
  getPath a b = findShortestPath walkways a b
  getDistance a b = pred $ length $ getPath a b
  getDoors a b = map getC $ filter isDoor $ map (\x -> tiles ! x) $ getPath a b
  getKeys a b = map getC $ filter isKey $ map (\x -> tiles ! x) $ getPath a b

  -- could optimize since ways should be symmetrical
  allPairs       = [ (a, b) | a <- allStartpoints, b <- allEndpoints, a /= b ]
  allStartpoints = Map.keys $ Map.filter (\x -> isKey x || isEntrance x) tiles
  allEndpoints   = Map.keys $ Map.filter isKey tiles
  walkways       = Map.filter isWalkable tiles


day18b :: String -> Int
day18b = undefined
