module AOC2019.Day8
  ( day8a
  , day8b
  , day8run
  )
where

import           Data.List.Split                          ( chunksOf )
import           Data.List                                ( minimumBy
                                                          , transpose
                                                          )
import           Data.Char                                ( digitToInt )

day8run :: IO ()
day8run = do
  contents <- readFile "input/day8"
  putStr "Day 8 - Part 1: "
  print $ day8a contents
  putStrLn "Day 8 - Part 2: "
  putStrLn $ day8b contents
  putStrLn ""


type Size = (Int, Int)

data Color = Black | White | Transparent deriving (Eq)
decodeColor :: Int -> Color
decodeColor 0 = Black
decodeColor 1 = White
decodeColor 2 = Transparent
decodeColor _ = error "Color undefined"

instance Show Color where
  show Black       = " "
  show White       = "\x2588"
  show Transparent = " "

type Pixel = Color
type Layer = [Pixel]

showLayer :: Size -> Layer -> String
showLayer (width, height) l = unlines rows
  where rows = map (concat . map show) $ chunksOf width l


data SIF = SIF {
  size :: Size,
  layers :: [Layer]
  } deriving (Eq, Show)

newSif :: Size -> [Char] -> SIF
newSif s@(width, height) contents = SIF s layers
 where
  pixels = map (decodeColor . digitToInt) contents
  layers = chunksOf (width * height) pixels


day8a :: String -> Int
day8a contents = digit White lz * digit Transparent lz
 where
  ls = layers $ newSif (25, 6) contents
  lz = snd $ minimumBy (\(a, _) (b, _) -> compare a b) $ zip
    (map (digit Black) ls)
    ls
  digit x = length . filter (== x)


day8b :: String -> String
day8b contents = showLayer (25, 6) $ cp
 where
  ls = layers $ newSif (25, 6) contents
  ps = transpose ls -- [Pixels] for each pixel
  cp = map (foldl combinePx Transparent) ps

combinePx :: Pixel -> Pixel -> Pixel
combinePx Transparent x = x
combinePx x           _ = x

