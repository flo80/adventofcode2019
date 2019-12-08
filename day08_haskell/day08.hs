import Data.List.Split (chunksOf)
import Data.List (minimumBy, transpose)
import Data.Char (digitToInt)

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  -- putStr "Part2: "
  -- print $ part2 $ contents
  
type Size = (Int,Int)

data Color = Black | White | Transparent deriving (Eq, Show)
decodeColor :: Int -> Color
decodeColor 0 = Black
decodeColor 1 = White
decodeColor 2 = Transparent
decodeColor _ = error "Color undefined"


type Pixel = Color
type Layer = [Pixel]

data SIF = SIF {
  size :: Size,
  layers :: [Layer]
  } deriving (Show, Eq)

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
part2 contents = undefined
    where
      ls = layers $ newSif (25,6) contents
      ps = transpose ls -- [Pixels] for each pixel

example2 = "0222112222120000"
testExample2 = (part2 example2) == [Black,White,White,Black]
