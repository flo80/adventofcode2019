module AOC2019.Day19
  ( day19a
  , day19b
  , day19run
  )
where

import           AOC2019.IntCodeComputer                  ( interactiveComputer
                                                          )
import           Data.List.Split                          ( chunksOf )
import           Data.List                                ( all )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.State
import           Debug.Trace

day19run :: IO ()
day19run = do
  contents <- readFile "input/day19"
  putStr "Day 19 - Part 1: "
  print $ day19a contents
  putStr "Day 19 - Part 2: "
  print $ day19b contents
  putStrLn ""


day19a :: String -> Int
day19a = sum . getDroneOutput1

getDroneOutput1 :: String -> [Int]
getDroneOutput1 contents = concatMap
  (\(x, y) -> interactiveComputer contents [x, y])
  [ (x, y) | y <- [0 .. 49], x <- [0 .. 49] ]


day19b :: String -> Int
day19b contents = (10000 * x + y)
 where
    -- setInitialState contents Map.empty
  (x, y) = findArea (0, 10)

  findArea :: Pos -> Pos
  findArea (testX, currY) = result
   where
        -- firstX follow the line to reduce search
    (firstX, _)  = findFirstX (testX, currY)
    (lastX , _)  = findLastX (firstX, currY)
    searchResult = searchArea firstX lastX currY

    result       = case (lastX - firstX) < 10 of
      True  -> findArea (firstX, currY + 1)
      False -> case searchResult of
        Just pos -> pos
        Nothing  -> findArea (firstX, currY + 1)


  searchArea :: Int -> Int -> Int -> Maybe Pos
  searchArea currX lastX currY | currX + 10 > lastX = Nothing
                               | otherwise          = result
   where
    searchField =
      [ (x, y) | y <- [currY .. (currY + 9)], x <- [currX .. (currX + 9)] ]
    searchResults = map getDroneOutput searchField
    result        = case all (== 1) searchResults of
      True  -> Just (currX, currY)
      False -> searchArea (currX + 1) lastX currY


  findFirstX :: Pos -> Pos
  findFirstX (testX, currY) = case getDroneOutput (testX, currY) == 1 of
    True  -> (testX, currY)
    False -> findFirstX ((testX + 1), currY)

  findLastX :: Pos -> Pos
  findLastX (testX, currY) = result
   where
    test   = getDroneOutput (testX, currY)

    result = case test == 0 of
      True  -> (testX - 1, currY)
      False -> findLastX ((testX + 1), currY)


  getDroneOutput :: Pos -> Int
  getDroneOutput (x, y) =
    head $ memoize 2 $ (interactiveComputer contents $ traceShowId  [x, y])

type Pos = (Int, Int)
