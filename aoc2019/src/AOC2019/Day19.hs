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
import           Data.Maybe                               ( fromMaybe )
import           Control.Monad.State
import           Debug.Trace
import           System.Console.ANSI

day19run :: IO ()
day19run = do
  contents <- readFile "input/day19"
  putStr "Day 19 - Part 1: "
  print $ day19a contents
  putStr "Day 19 - Part 2: "
  print $ day19b contents
  putStrLn ""


showField :: String -> Int -> Int -> String
showField contents maxX maxY = concatMap (\l -> "\n" ++ concatMap show l) lines
 where
  s = concatMap (\(x, y) -> interactiveComputer contents [x, y])
                [ (x, y) | y <- [0 .. (maxY - 1)], x <- [0 .. (maxX - 1)] ]
  lines = chunksOf maxX s


day19a :: String -> Int
day19a = sum . getDroneOutput1

getDroneOutput1 :: String -> [Int]
getDroneOutput1 contents = concatMap
  (\(x, y) -> interactiveComputer contents [x, y])
  [ (x, y) | y <- [0 .. 49], x <- [0 .. 49] ]


type Pos = (Int, Int)

day19b :: String -> Int
day19b contents = (10000 * x + y)
 where
    -- setInitialState contents Map.empty
  (x, y) = findArea (0, 10) Nothing

  findArea :: Pos -> Maybe Int -> Pos
  findArea (prevFirstX, currY) prevLastX = result
   where
    testLastX    = fromMaybe firstX $ prevLastX
        -- firstX follow the line to reduce search
    (firstX, _)  = findFirstX (prevFirstX, currY)
    (lastX , _)  = findLastX (testLastX, currY)
    searchResult = searchArea firstX lastX currY

    result       = case (lastX - firstX) < 100 of
      True  -> findArea (firstX, currY + 1) (Just lastX)
      False -> case searchResult of
        Just pos -> pos
        Nothing  -> findArea (firstX, currY + 1) (Just lastX)


  searchArea :: Int -> Int -> Int -> Maybe Pos
  searchArea currX lastX currY | currX + 10 > lastX = Nothing
                               | otherwise          = result
   where
    searchField =
      [ (x, y) | y <- [currY , (currY + 99)], x <- [currX , (currX + 99)] ]
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
  getDroneOutput (x, y) = head $ (interactiveComputer contents [x, y])

