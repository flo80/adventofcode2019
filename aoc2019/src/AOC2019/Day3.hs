-- rewrite of day3 based on https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md#day-3
module AOC2019.Day3
  ( day3a
  , day3b
  , day3run
  )
where

import           Data.List.Split
import           Linear.V2
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map

day3run :: IO ()
day3run = do
  contents <- readFile "input/day3"
  putStr "Day 3 - Part 1: "
  print $ day3a contents
  putStr "Day 3 - Part 2: "
  print $ day3b contents
  putStrLn ""


type Point = V2 Int

parseString :: String -> [Point]
parseString = concatMap parsePoint . splitOn ","
 where
  parsePoint (d : ns) = replicate (read ns) $ case d of
    'U' -> V2 0 1
    'D' -> V2 0 (-1)
    'R' -> V2 1 0
    'L' -> V2 (-1) 0

visited :: [Point] -> Set Point
visited = Set.fromList . scanl1 (+)

day3a :: String -> Int
day3a content = minimum $ Set.map dist $ Set.intersection as bs
 where
  [as, bs] = map (visited . parseString) (lines content)
  dist (V2 x y) = abs x + abs y

visited2 :: [Point] -> Map Point Int
visited2 = Map.fromListWith min . flip zip [1 ..] . scanl1 (+)

day3b :: String -> Int
day3b content = minimum (Map.intersectionWith (+) as bs)
  where [as, bs] = map (visited2 . parseString) (lines content)
