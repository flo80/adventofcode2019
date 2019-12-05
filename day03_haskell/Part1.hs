import System.IO
import Data.List.Split
import qualified Data.Set as Set

type Position = (Int, Int)
data Direction = U | D | R | L deriving (Eq, Show)
type Movement = (Direction, Int)

startingPosition :: Position
startingPosition = (0,0)

getPositions :: Position -> Movement -> [Position]
getPositions (x, y) (dir, length) =
  case dir of U -> [(x, ny) | ny <- [y..(y + length)] ]
              D -> reverse [(x, ny) | ny <- [(y - length)..y] ]
              R -> [(nx, y) | nx <- [x..(x + length)] ]
              L -> reverse [(nx, y) | nx <- [(x - length)..x] ]

getAllPositions :: Position -> [Movement] -> [Position]
getAllPositions start [] = [start]
getAllPositions start (m:ms) = 
  let
    currentPositions = getPositions start m
    end = last currentPositions
  in 
    tail currentPositions ++ getAllPositions end ms

getMinimumOverlap :: ([Position], [Position]) -> Int
getMinimumOverlap (p1, p2) =
  let 
    set1 = Set.fromList p1
    set2 = Set.fromList p2
    overlaps = Set.toList $ Set.intersection set1 set2
    dists = map calcdist overlaps
      where calcdist (a, b) = abs a + abs b
  in
    minimum dists

getMinimum :: ([Movement],[Movement]) -> Int
getMinimum (m1, m2) = 
  getMinimumOverlap (getAllPositions startingPosition m1, getAllPositions startingPosition m2)

parseString :: String -> [Movement]
parseString s =
  let ms = splitOn "," s
  in map parseMovement ms

parseMovement :: String -> Movement
parseMovement m = (dir, dist)
  where 
    dist = read $ tail m 
    dir = case head m of 
      'U' -> U
      'D' -> D
      'L' -> L
      'R' -> R   

examples :: [(String,String,Int)]
examples = [
  ("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83",159),
  ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",135)
  ]

check_examples :: [(String,String,Int)] -> [Bool]
check_examples = map check_example 

check_example :: (String, String, Int) -> Bool
check_example (s1, s2, r) =
  let 
    m1 = parseString s1
    m2 = parseString s2
  in 
    getMinimum (m1,m2) == r


main = do
  contents <- readFile "input"
  let ms = map parseString $ lines $ contents
  let m1 = head ms
  let m2 = last ms
  print $ getMinimum (m1, m2)




exampleStrings = ("R8,U5,L5,D3","U7,R6,D4,L4")

example :: ([Movement],[Movement])
example = ([(R,8),(U,5),(L,5),(D,3)],
           [(U,7),(R,6),(D,4),(L,4)]
          )

test_parse :: Bool
test_parse = ((parseString $ fst exampleStrings) == fst example) && ((parseString $ snd exampleStrings) == snd example)
