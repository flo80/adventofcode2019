module AOC2019.Day6
  ( day6a
  , day6b
  , day6run
  )
where

import Data.List.Split
import qualified Data.Set as Set

day6run :: IO ()
day6run = do
  contents <- readFile "input/day6"
  putStr "Day 6 - Part 1: "
  print $ day6a contents
  putStr "Day 6 - Part 2: "
  print $ day6b contents
  putStrLn ""


parseInput :: String -> String
parseInput = id

day6a :: String -> Int
day6a contents =  let
    lineItems = lines $ contents
    allPairs = map splitOrbitString lineItems
  in 
    recurseOrbits allPairs

splitOrbitString :: String -> (String, String)
splitOrbitString s = 
  let (a:b:_) = splitOn ")" s
  in (a,b)

recurseOrbits :: [(String, String)] -> Int 
recurseOrbits allPairs =
  recurseOrbits' allPairs 0 "COM"

recurseOrbits' :: [(String, String)] -> Int -> String -> Int
recurseOrbits' allPairs level currentObject  =
  let
    children = filter (\(a,_) -> a == currentObject) allPairs :: [(String, String)]
  in case (length children) of
    0 -> level 
    _ -> level + (sum $ map (\(_,c) -> recurseOrbits' allPairs (level + 1) c) children)


day6b :: String -> Int
day6b contents = 
  let
    lineItems = lines $ contents
    allPairs = map splitOrbitString lineItems
    youPred = getAllPredecessors allPairs "YOU"
    sanPred = getAllPredecessors allPairs "SAN"
    common = length $ takeWhile (\x -> elem x youPred) sanPred
  in 
    length youPred + length sanPred - 2 * common - 2

getAllPredecessors :: [(String,String)] -> String -> [String]
getAllPredecessors allPairs currentObject 
    | currentObject == "COM"  = ["COM"]
    | otherwise               = getAllPredecessors allPairs parent ++ [currentObject] 
    where
      parent = fst $ head $ filter (\(_,b) -> b == currentObject ) allPairs




-- Full data structure

data OrbitalObject = 
  OrbitalObject { name :: String
                , predecessors :: Int
                , children :: [OrbitalObject]
                } deriving (Show)

newSolarSystem :: [(String, String)] -> OrbitalObject  
newSolarSystem allPairs =
  buildOrbital' allPairs 0 "COM" 

buildOrbital' ::[(String, String)] -> Int -> String -> OrbitalObject  
buildOrbital' allPairs predecessors currentObject =
  let
    childrenList = filter (\(a,_) -> a == currentObject) allPairs 
    children = map (\(_,c) -> buildOrbital' allPairs (predecessors + 1) c) childrenList
  in 
    OrbitalObject { name = currentObject
                  , predecessors = predecessors
                  , children = children
                  }

getAllOrbits :: OrbitalObject -> Int
getAllOrbits com 
  | (length $ children com) == 0  = predecessors com
  | otherwise                     = predecessors com + (sum $ map getAllOrbits $ children com)

listOrbitNumbers' :: OrbitalObject -> [(String, Int)]
listOrbitNumbers' com  
  | (length $ children com) == 0  = [(name com, predecessors com)]
  | otherwise                     = [(name com, predecessors com)] ++ (concatMap listOrbitNumbers' $ children com)
