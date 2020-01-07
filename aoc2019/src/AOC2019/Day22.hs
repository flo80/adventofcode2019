module AOC2019.Day22
  ( day22a
  , day22b
  , day22run
  )
where

import           AOC.Common                               ( egcd )
import           Data.List                                ( foldl' )

day22run :: IO ()
day22run = do
  contents <- readFile "input/day22"
  putStr "Day 22 - Part 1: "
  print $ day22a contents
  putStr "Day 22 - Part 2: "
  print $ day22b contents
  putStrLn ""

type Shuffle = (Integer, Integer)

-- from https://github.com/ephemient/aoc2019/blob/hs/src/Day22.hs
shuffleTimes :: Integer -> [(Shuffle -> Shuffle)] -> Integer -> Shuffle
shuffleTimes decksize moves times = mpow decksize
                                         (foldl' (\a f -> f a) (1, 0) moves)
                                         times

 where
  mpow d (m, a) t | t < 0 = mpow d (n, -n * a `mod` d) (-t)
    where (n, _, 1) = egcd m d
  mpow _ _ 0 = (1, 0)
  mpow _ z 1 = z
  mpow d z t = f $ mpow d (mmul d z z) q
   where
    (q, r) = t `divMod` 2
    f      = if r == 0 then id else mmul d z

  mmul d (a, b) (a', b') = (a * a' `mod` d, (a * b' + b) `mod` d)


dealIntoNewStack :: Integer -> Shuffle -> Shuffle
dealIntoNewStack d (m, a) = ((-m) `mod` d, (-a - 1) `mod` d)

cutNCards :: Integer -> Integer -> Shuffle -> Shuffle
cutNCards d n (m, a) = (m, (a - n) `mod` d)

dealWithIncrementN :: Integer -> Integer -> Shuffle -> Shuffle
dealWithIncrementN d n (m, a) = (m * n `mod` d, a * n `mod` d)


parseInput :: Integer -> String -> [(Shuffle -> Shuffle)]
parseInput decksize input = map (parseLine decksize . words) $ lines input

parseLine :: Integer -> [String] -> (Shuffle -> Shuffle)
parseLine decksize ["deal", "into", "new", "stack"] = dealIntoNewStack decksize
parseLine decksize ["deal", "with", "increment", n] =
  dealWithIncrementN decksize $ read n
parseLine decksize ["cut", n] = cutNCards decksize $ read n


day22a :: String -> Integer
day22a contents = (m * 2019 + a) `mod` d
 where
  d      = 10007
  (m, a) = shuffleTimes d (parseInput d contents) 1


day22b :: String -> Integer
day22b contents = (m * finalPos + a) `mod` decksize
 where
  decksize = 119315717514047
  times    = -101741582076661
  finalPos = 2020
  moves    = parseInput decksize contents
  (m, a)   = shuffleTimes decksize moves times
