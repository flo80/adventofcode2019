import Data.List.Split (chunksOf)
import Data.List (minimumBy)
import Data.Char (digitToInt)

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  -- putStr "Part2: "
  -- print $ part2 $ contents
  
type Size = (Int,Int)
type Pixel = Int
type Row = [Pixel]
type Layer = [Row]

data SIF = SIF {
  size :: Size,
  layers :: [Layer]
  } deriving (Show, Eq)

newSif :: Size -> [Char] -> SIF
newSif s@(width, height) contents =
  SIF s layers
  where
    pixels = map digitToInt contents 
    rows = chunksOf width pixels
    layers = chunksOf height rows

-- PART 1 
part1 :: String -> Int
part1 contents =
  digit 1 lz * digit 2 lz
  where 
    ls = layers $ newSif (25,6) contents
    lz = snd $ minimumBy (\(a,_) (b,_) -> compare a b ) $ zip (map (digit 0) ls) ls
    digit x = length . filter (== x) . concat 

