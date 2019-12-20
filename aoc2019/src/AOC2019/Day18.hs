{-# LANGUAGE RecordWildCards #-}

module AOC2019.Day18
  ( day18a
  , day18b
  , day18run
  )
where

import           Algorithm.Search
import           AOC.Common
import           Data.Maybe                               ( fromJust )
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
data Player = Player {position:: Position, keys :: Set Char} deriving (Show, Ord)
instance Eq Player where
  (==) a b = position a == position b && keys a == keys b


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


day18a :: String -> Int
day18a contents = fst $ fromJust $ dijkstra getNeighbors
                                            getCost
                                            (\a -> allKeys == keys a)
                                            player
 where
  tiles             = parseInput contents
  (ks, missingKeys) = listKeys tiles
  allKeys           = Set.fromList ks
  allD              = allDistances tiles
  player            = Player (tiles ?! Entrance) Set.empty

  getCost :: Player -> Player -> Int
  getCost a b = cost
   where
    (cost, _, _) = allD ! (pa, pb)
    pa           = position a
    pb           = position b

  getNeighbors :: Player -> [Player]
  getNeighbors player = map newPlayer possibleNextHops
   where
    possibleNextHops :: [((Position, Position), (Int, [Char], [Char]))]
    possibleNextHops = Map.toList $ Map.filterWithKey
      (\(a, b) (_, requiredKeys, gainedKeys) ->
        a
          == (position player)
          && any (\k -> Set.member k missingKeys) gainedKeys
          && all (\k -> Set.member k haveKeys) requiredKeys
      )
      allD
    newPlayer ((_, dest), (_, _, newKeys)) =
      Player dest (Set.union haveKeys $ Set.fromList newKeys)
    haveKeys    = keys player
    missingKeys = Set.difference allKeys haveKeys


-- calculate  between a,b, distance, doors (i.e. keys required), keys on the way
allDistances :: Tiles -> Map (Position, Position) (Int, [Char], [Char])
allDistances tiles = Map.fromList
  $ map (\(a, b) -> ((a, b), (getData a b))) allPairs
 where
  getData a b = (cost, doors, keys)
   where
      --Position -> Position -> Maybe (cost, [Position])
    (cost, path) = fromJust $ dijkstra getNeighbors (\_ _ -> 1) (== b) a
    doors        = map getC $ filter isDoor $ map (\x -> tiles ! x) $ path
    keys         = map getC $ filter isKey $ map (\x -> tiles ! x) $ path

  getNeighbors pos =
    filter (\pos -> isWalkable $ tiles ! pos) $ map (+ pos) adjacentPositions

  -- could optimize since ways should be symmetrical
  allPairs       = [ (a, b) | a <- allStartpoints, b <- allEndpoints, a /= b ]
  allStartpoints = Map.keys $ Map.filter (\x -> isKey x || isEntrance x) tiles
  allEndpoints   = Map.keys $ Map.filter isKey tiles
  walkways       = Map.filter isWalkable tiles

  isEntrance Entrance = True
  isEntrance _        = False
  isKey (Key _) = True
  isKey _       = False
  isDoor (Door _) = True
  isDoor _        = False
  isWalkable Wall = False
  isWalkable _    = True

day18b :: String -> Int
day18b = undefined
