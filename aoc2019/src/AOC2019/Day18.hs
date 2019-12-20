{-# LANGUAGE RecordWildCards #-}

module AOC2019.Day18
  ( day18a
  , day18b
  , day18run
  )
where

import           Algorithm.Search
import           AOC.Common
import           Data.Maybe                               ( fromJust
                                                          , catMaybes
                                                          )
import           Data.Foldable                            ( toList )
import           Data.Function                            ( on )
import           Data.List                                ( minimumBy
                                                          , sort
                                                          , (\\)
                                                          )
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

-- State for search
data Player = Player {position:: Position, keys :: Set Char} deriving (Show, Ord)
instance Eq Player where
  (==) a b = position a == position b && keys a == keys b

-- create dedicated position for each quadrant
data Player4 = Player4{positions::(Position,Position,Position,Position), keys4 :: Set Char} deriving (Show, Eq, Ord)

-- World 
type Tiles = Map Position Tile
data Tile = Wall | Open | Entrance | Door Char | Key Char deriving (Eq)
instance Show Tile where
  show Wall     = "#"
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
  allD              = allDistances tiles Nothing
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
allDistances
  :: Tiles -> Maybe Position -> Map (Position, Position) (Int, [Char], [Char])
allDistances tiles midpoint = Map.fromList
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
  -- optimized for same quadrants / also fromJust for dijkstra should hold then
  allPairs =
    [ (a, b)
    | a <- allStartpoints
    , b <- allEndpoints
    , a /= b
    , quadrant a == quadrant b
    ]
  allStartpoints = Map.keys $ Map.filter (\x -> isKey x || isEntrance x) tiles
  allEndpoints   = Map.keys $ Map.filter isKey tiles
  walkways       = Map.filter isWalkable tiles

  quadrant :: Position -> Int
  quadrant (V2 x y) = case midpoint of
    Nothing         -> 0
    Just (V2 mx my) -> ud + lr
     where
      ud = if y < my then 1 else 3
      lr = if x < mx then 0 else 1

  isEntrance Entrance = True
  isEntrance _        = False
  isKey (Key _) = True
  isKey _       = False
  isDoor (Door _) = True
  isDoor _        = False
  isWalkable Wall = False
  isWalkable _    = True

day18b :: String -> Int
day18b contents = fst $ fromJust $ dijkstra getNeighbors
                                            getCost
                                            (\a -> allKeys == keys4 a)
                                            player
 where
  (ks, missingKeys) = listKeys tiles
  allKeys           = Set.fromList ks
  allD              = allDistances tiles (Just midPoint)
  player            = Player4 (q1, q2, q3, q4) Set.empty

  -- Apply changes to map 
  (tiles, midPoint, [q1, q2, q3, q4]) =
    ( Map.unions [newWall, newEntrances, originalMap]
    , originalEntrance
    , newEntrancesList
    )
   where
    originalMap      = parseInput contents
    originalEntrance = originalMap ?! Entrance
    newWall =
      Map.fromList
        $ map (\p -> (p, Wall))
        $ map ((+) originalEntrance)
        $ (V2 0 0)
        : adjacentPositions
    newEntrances     = Map.fromList $ map (\p -> (p, Entrance)) newEntrancesList
    newEntrancesList = map
      ((+) originalEntrance)
      [(V2 (-1) (-1)), (V2 (1) (-1)), (V2 (-1) (1)), (V2 (1) (1))]

  getCost :: Player4 -> Player4 -> Int
  getCost a b = sum $ map getCost' delta
   where
    delta            = [(a1, b1), (a2, b2), (a3, b3), (a4, b4)]
    (a1, a2, a3, a4) = positions a
    (b1, b2, b3, b4) = positions b
    getCost' (pa, pb) = case pa == pb of
      True  -> 0
      False -> cost where (cost, _, _) = allD ! (pa, pb)


  getNeighbors :: Player4 -> [Player4]
  getNeighbors player = concatMap
    (\(pos, options) -> map (newPlayer4 pos) options)
    allPossibleNextHops
   where
    allPossibleNextHops
      :: [(Position, [((Position, Position), (Int, [Char], [Char]))])]
    allPossibleNextHops =
      map (\pos -> (pos, possibleNextHops pos)) [o1, o2, o3, o4]

    possibleNextHops
      :: Position -> [((Position, Position), (Int, [Char], [Char]))]
    possibleNextHops position = Map.toList $ Map.filterWithKey
      (\(a, b) (_, requiredKeys, gainedKeys) ->
        a
          == position
          && any (\k -> Set.member k missingKeys) gainedKeys
          && all (\k -> Set.member k haveKeys) requiredKeys
      )
      allD

    newPlayer4
      :: Position -> ((Position, Position), (Int, [Char], [Char])) -> Player4
    newPlayer4 pos ((_, dest), (_, _, newKeys)) = Player4
      newDest
      (Set.union haveKeys $ Set.fromList newKeys)
     where
      -- to avoid searches which will fail always stay in quadrant
      newDest = case quadrant dest of
        1 -> (dest, o2, o3, o4)
        2 -> (o1, dest, o3, o4)
        3 -> (o1, o2, dest, o4)
        4 -> (o1, o2, o3, dest)

    haveKeys         = keys4 player
    missingKeys      = Set.difference allKeys haveKeys
    (o1, o2, o3, o4) = positions player

  quadrant :: Position -> Int
  quadrant (V2 x y) = ud + lr
   where
    ud         = if y < my then 1 else 3
    lr         = if x < mx then 0 else 1
    (V2 mx my) = midPoint
