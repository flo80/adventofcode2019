module AOC.Common
  ( Position
  , parseCharacterArrayFromString
  , parseTilesFromString
  , parseTilesFromStringWith
  , getTilesPicture
  , (?!)
  , (?)
  , adjacentPositions
  , egcd
  )
where

import           Data.List                                ( concat )
import           Data.Maybe                               ( fromMaybe )
import           Data.Map                                 ( Map
                                                          , (!)
                                                          )
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
  gt pos = fromMaybe " " $ fmap show $ Map.lookup pos tiles

adjacentPositions :: [Position]
adjacentPositions = [V2 (0) (-1), V2 (0) (1), V2 (1) (0), V2 (-1) (0)]

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


egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a 0 = (1, 0, a)
egcd a b = (t, s - q * t, abs g) where
  (q, r)    = a `quotRem` b
  (s, t, g) = egcd b r
