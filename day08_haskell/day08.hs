import Data.List.Split (chunksOf)
import Data.List (minimumBy, transpose)
import Data.Char (digitToInt)

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  putStrLn "Part2: "
  putStrLn $ showLayer (25,6) $ part2 $ contents
  
type Size = (Int,Int)

data Color = Black | White | Transparent deriving (Eq)
decodeColor :: Int -> Color
decodeColor 0 = Black
decodeColor 1 = White
decodeColor 2 = Transparent
decodeColor _ = error "Color undefined"

instance Show Color where
  show Black = " "
  show White = "#"
  show Transparent = " "

type Pixel = Color
type Layer = [Pixel]  

showLayer :: Size -> Layer -> String
showLayer (width,height) l =
  unlines rows 
  where
    rows = map (concat . map show) $ chunksOf width l 


data SIF = SIF {
  size :: Size,
  layers :: [Layer]
  } deriving (Eq, Show)

newSif :: Size -> [Char] -> SIF
newSif s@(width, height) contents =
  SIF s layers
  where
    pixels = map (decodeColor . digitToInt) contents 
    layers = chunksOf (width * height) pixels

-- PART 1 
part1 :: String -> Int
part1 contents =
  digit White lz * digit Transparent lz
  where 
    ls = layers $ newSif (25,6) contents
    lz = snd $ minimumBy (\(a,_) (b,_) -> compare a b ) $ zip (map (digit Black) ls) ls
    digit x = length . filter (== x)  

-- PART 1
part2 :: String -> Layer
part2 contents = cp
    where
      ls = layers $ newSif (25,6) contents
      ps = transpose ls -- [Pixels] for each pixel
      cp = map (foldl combinePx Transparent) ps

combinePx :: Pixel -> Pixel -> Pixel
combinePx Transparent x = x
combinePx x _ = x

example2 = "0222112222120000"
testExample2 = (part2 example2) == [Black,White,White,Black]
