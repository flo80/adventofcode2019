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

day22run :: IO ()
day22run = do
  contents <- readFile "input/day22"
  putStr "Day 22 - Part 1: "
  print $ day22a contents
  putStr "Day 22 - Part 2: "
  print $ day22b contents
  putStrLn ""

type Shuffle = (Int -> Int)

shuffle :: Int -> [Shuffle] -> Int
shuffle pos moves = foldl (\d f -> f d) pos moves

dealIntoNewStack :: Int -> Shuffle
dealIntoNewStack decksize pos = (-1) * (pos + 1) `mod` decksize

cutNCards :: Int -> Int -> Shuffle
cutNCards decksize n pos = (pos - n) `mod` decksize

dealWithIncrementN :: Int -> Int -> Shuffle
dealWithIncrementN decksize n pos = (n * pos) `mod` decksize


parseInput :: Int -> String -> [Shuffle]
parseInput decksize input = map (parseLine decksize . words) $ lines input

parseLine :: Int -> [String] -> Shuffle
parseLine decksize ["deal", "into", "new", "stack"] = dealIntoNewStack decksize
parseLine decksize ["deal", "with", "increment", n] =
  dealWithIncrementN decksize $ read n
parseLine decksize ["cut", n] = cutNCards decksize $ read n


day22a :: String -> Int
day22a = shuffle 2019 . parseInput 10007

day22b :: String -> Int
day22b contents = undefined
