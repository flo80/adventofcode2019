module AOC2019.Day20
  ( day20a
  , day20b
  , day20run
  )
where

import           AOC.Common
-- Switch to 3rd party search algorithms
import           Algorithm.Search
import           Data.Maybe                               ( fromJust
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
parseInput contents = (tiles, start, end)
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
day20a contents = fst $ fromJust $ dijkstra (getNeighbors)
                                            (\_ _ -> 1)
                                            (== end)
                                            start
 where
  (tiles, start, end) = parseInput contents
  getNeighbors pos = filter (\p -> Map.member p tiles) $ neighbors pos

  neighbors :: Position -> [Position]
  neighbors pos = case t of
    Portal to -> to : getNeighbors'
    Walkway   -> getNeighbors'
   where
    t             = tiles ! pos
    getNeighbors' = filter (\p -> Map.member p tiles) nb
    nb            = map (+ pos) [up, down, left, right]



day20b :: String -> Int
day20b contents =
  fst
    $ fromJust
  -- 42 heuristics from https://www.reddit.com/r/adventofcode/comments/ed5ei2/2019_day_20_solutions/fbg5pmk/
    $ aStar (getNeighbors)
            (\_ _ -> 1)
            (\(p, l) -> max 0 (l - 1) * 42)
            (== (end, 0))
            (start, 0)
 where
  (tiles, start, end) = parseInput contents
  (maxX, maxY) = foldl (\(ax, ay) (V2 nx ny) -> (max ax nx, max ay ny)) (0, 0)
    $ Map.keys tiles

  getNeighbors (pos, level) =
    filter (\(p, l) -> Map.member p tiles) $ neighbors (pos, level)

  -- position and level 
  neighbors :: (Position, Int) -> [(Position, Int)]
  neighbors (pos, level) = case t of
    -- check where portal leads to
    Portal (V2 x y) -> case min x y < 5 || x > (maxX - 5) || y > (maxY - 5) of
      True  -> (V2 x y, level + 1) : getNeighbors' -- leads to outer ring, i.e. is inner portals
      False -> case level == 0 of -- outer portal leading inside
        True  -> getNeighbors' -- level 0 outer portals closed
        False -> (V2 x y, level - 1) : getNeighbors'

    -- on levels below 0, AA and ZZ are walls
    Walkway -> case level == 0 of
      False -> filter (\(p, _) -> p `notElem` [start, end]) $ getNeighbors'
      True  -> getNeighbors'
   where
    t = tiles ! pos
    getNeighbors' =
      map (\p -> (p, level)) $ filter (\p -> Map.member p tiles) nb
    nb = map (+ pos) [up, down, left, right]


