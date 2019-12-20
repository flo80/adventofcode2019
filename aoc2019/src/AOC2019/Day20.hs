module AOC2019.Day20
  ( day20a
  , day20b
  , day20run
  )
where

import           AOC.Common
import           Control.Applicative
import           Data.Maybe                               ( fromMaybe
                                                          , fromJust
                                                          , catMaybes
                                                          )
import           Data.Char                                ( isUpper )
import           Data.List                                ( and
                                                          , sortOn
                                                          , init
                                                          , groupBy
                                                          )
import           Data.Map                                 ( Map
                                                          , (!?)
                                                          , (!)
                                                          )
import qualified Data.Map                      as Map
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Linear.V2


day20run :: IO ()
day20run = do
  contents <- readFile "input/day20"
  putStr "Day 20 - Part 1: "
  print $ day20a contents
  putStr "Day 20 - Part 2: "
  print $ day20b contents
  putStrLn ""


type Tiles = Map Position Tile
data Tile = Portal Position | Walkway deriving Show
type Direction = Position

up :: Direction
up = V2 0 (-1)
down :: Direction
down = V2 0 1
left :: Direction
left = V2 (-1) 0
right :: Direction
right = V2 1 0



parseInput :: String -> (Tiles, Position, Position)
parseInput contents =  (tiles, start, end)
 where
  charMap  = Map.filter (/= '#') $ parseCharacterArrayFromString contents
  walkways = Map.mapWithKey (\pos _ -> Walkway) $ Map.filter (== '.') charMap
  portals' = sortOn snd $ catMaybes $ map getPortal $ Map.keys $ Map.filter
    isUpper
    charMap
  start = fst $ head portals'
  end   = fst $ last portals'
  portals =
    Map.fromList
      $ concatMap (\[(aP, _), (bP, _)] -> [(aP, Portal bP), (bP, Portal aP)])
      $ groupBy (\a b -> snd a == snd b)
      $ init
      $ tail
      $ portals'

  tiles = Map.union portals walkways

  getPortal :: Position -> Maybe (Position, String)
  getPortal pos | length res == 0 = Nothing
                | length res == 1 = Just (head res)
                | otherwise       = error $ "two portals in pos " ++ show pos
   where
    res =
      catMaybes
        $  map (\d -> getPortal' pos (pos + d) (pos + d + d)) [down, right]
        ++ map (\d -> getPortal' pos (pos - d) (pos + d))     [up, left]

    getPortal' :: Position -> Position -> Position -> Maybe (Position, String)
    getPortal' a b c | isPortal aV bV cV = Just (c, catMaybes [aV, bV])
                     | otherwise         = Nothing
     where
      aV = charMap !? a
      bV = charMap !? b
      cV = charMap !? c
    isPortal :: Maybe Char -> Maybe Char -> Maybe Char -> Bool
    isPortal (Just aV) (Just bV) (Just cV) =
      and [isUpper aV, isUpper bV, cV == '.']
    isPortal _ _ _ = False



day20a :: String -> Int
day20a contents = findDistance tiles end start
  where (tiles, start, end) = parseInput contents


neighbors :: Tiles -> Position -> [Position]
neighbors tiles pos = case t of
  Portal to -> to : getNeighbors'
  Walkway   -> getNeighbors'
 where
  t             = tiles ! pos
  getNeighbors' = Map.keys $ Map.filterWithKey (\k _ -> k `elem` nb) tiles
  nb            = map (+ pos) [up, down, left, right]

findDistance :: Tiles -> Position -> Position -> Int
findDistance tiles destination start = findDistance'
  tiles
  (Map.singleton start 0)
  (Map.delete start $ Map.map (\_ -> 9999999) tiles)
  destination
  start
 where
  findDistance'
    :: Tiles
    -> Map Position Int
    -> Map Position Int
    -> Position
    -> Position
    -> Int
  findDistance' tiles visited unvisited destination current
    | current == destination = visited ! current
    | otherwise              = findDistance'
      tiles
      newVisited
      newUnvisited
      destination
      nextHop

   where
    nbList = neighbors tiles current
    nbDist :: Map Position Int
    nbDist =
      Map.map calcDist $ Map.filterWithKey (\k _ -> k `elem` nbList) unvisited

    ownDist = visited ! current
    calcDist d = min d (ownDist + 1)

    tempUnvisited :: Map Position Int
    tempUnvisited   = Map.union nbDist unvisited
    (nextHop, dist) = Map.foldlWithKey minDist
                                       ((V2 0 0), 9999999)
                                       tempUnvisited
     where
      minDist (aPos, aDist) bPos bDist = case (bDist < aDist) of
        True -> (bPos, bDist)
        flse -> (aPos, aDist)
    newUnvisited = Map.delete nextHop tempUnvisited
    newVisited   = Map.insert nextHop dist visited


day20b :: String -> Int
day20b = undefined
