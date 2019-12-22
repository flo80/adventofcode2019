module AOC2019.Day22
  ( day22a
  , day22b
  , day22run
  -- exports for unit testing
  , shuffle
  , cutNCards
  , dealIntoNewStack
  , dealWithIncrementN
  , parseInput
  )
where

import           Data.List                                ( sortOn, elemIndex )
import Data.Maybe (fromJust)

day22run :: IO ()
day22run = do
  contents <- readFile "input/day22"
  putStr "Day 22 - Part 1: "
  print $ day22a contents
  putStr "Day 22 - Part 2: "
  print $ day22b contents
  putStrLn ""

type Deck = [Int]
type Shuffle = (Deck -> Deck)

shuffle :: Deck -> [(Deck -> Deck)] -> Deck
shuffle deck moves  = foldl (\d f -> f d) deck moves

dealIntoNewStack :: Shuffle
dealIntoNewStack = reverse

cutNCards :: Int -> Shuffle
cutNCards x deck = b ++ a
 where
  (a, b) = splitAt pos deck
  pos    = mod x $ length deck

dealWithIncrementN :: Int -> Shuffle
dealWithIncrementN x deck =
  map fst $ sortOn snd $ zip deck $ map (`mod` length deck) $ [0, x ..]


parseInput :: String -> [Shuffle]
parseInput = map (parseLine . words) . lines

parseLine :: [String] -> Shuffle
parseLine ["deal", "into", "new"      , "stack"] = dealIntoNewStack
parseLine ["deal", "with", "increment", n      ] = dealWithIncrementN $ read n
parseLine ["cut", n]                             = cutNCards $ read n


day22a :: String -> Int
day22a = succ . fromJust . elemIndex 2019 . shuffle [0..10006] . parseInput 

day22b :: String -> Int
day22b contents = undefined