module AOC.Common
  ( Position
  , parseTilesFromString
  , parseTilesFromStringWith
  , getTilesPicture
  , (?!)
  , (?)
  , findShortestPathWith
  , findShortestPath
  )
where

import           Data.List                                ( concat )
import           Data.Maybe                               ( fromMaybe )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Linear.V2

type Position = V2 Int

parseCharacterArrayFromString :: String -> Map Position Char
parseCharacterArrayFromString input = Map.fromList $ map
  (\(x, y, c) -> ((V2 x y), c))
  [ (x, y, c) | (y, row) <- (zip [0 ..] ls), (x, c) <- (zip [0 ..] row) ]
 where
  ls :: [String]
  ls = lines input


parseTilesFromStringWith :: (Char -> a) -> String -> Map Position a
parseTilesFromStringWith fct = Map.map fct . parseCharacterArrayFromString

parseTilesFromString :: Enum a => String -> Map Position a
parseTilesFromString = parseTilesFromStringWith (toEnum . fromEnum)


getTilesPicture :: Show a => Map Position a -> String
getTilesPicture tiles = concatMap line [0 .. maxY]
 where
  (V2 maxX maxY) = maximum $ Map.keys tiles
  line y = (concat $ map gt [ (V2 x y) | x <- [0 .. maxX] ]) ++ "\n"
  gt pos = fromMaybe "?" $ fmap show $ Map.lookup pos tiles


-- get Position for a unique tile - if not found or not unique errors
infix 9 ?!
(?!) :: Eq a => Map Position a -> a -> Position
(?!) tiles tile = case length l of
  0 -> error "Tile not known"
  1 -> head l
  _ -> error "More than one element in Map with this type"
  where l = tiles ? tile

-- get Positions for a tile (can be empty list)
infix 9 ?
(?) :: Eq a => Map Position a -> a -> [Position]
(?) tiles tile = Map.keys $ Map.filter (== tile) tiles

-- find the shortest path between two positions, whereas the function determines if a tile can be used
findShortestPathWith
  :: Map Position a -> (a -> Bool) -> Position -> Position -> [Position]
findShortestPathWith tiles isWalkable start destination =
  findShortestPath (Map.filter isWalkable tiles) start destination

-- find the shortest path between two positions
findShortestPath :: Map Position a -> Position -> Position -> [Position]
findShortestPath tiles start destination = case length paths of
  0 -> []
  1 -> head paths
  _ -> foldl1 (\a x -> if length x < length a then x else a) paths
 where
  paths = findDistance' tiles [] destination start

  findDistance'
    :: Map Position a -> [Position] -> Position -> Position -> [[Position]]
  findDistance' world visited destination pos
    | pos == destination = [pos : visited]
    | otherwise = concatMap (findDistance' world newVisited destination)
                            relevantNeighbors
   where
    relevantNeighbors =
      filter (\k -> k `notElem` visited)
        $ filter ((flip $ Map.member) world)
        $ neighbors pos
    newVisited = pos : visited


-- get all neighboring positions (up, down, left right)
neighbors :: Position -> [Position]
neighbors pos = map (+ pos) [V2 (0) (-1), V2 (0) (1), V2 (1) (0), V2 (-1) (0)]
