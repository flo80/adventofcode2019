import System.IO
import Data.List.Split
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Position = (Int, Int)
data Direction = U | D | R | L deriving (Eq, Show)
type Movement = (Direction, Int)


-- Parser
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



exampleStrings = ("R8,U5,L5,D3","U7,R6,D4,L4")

example :: ([Movement],[Movement])
example = ([(R,8),(U,5),(L,5),(D,3)],
            [(U,7),(R,6),(D,4),(L,4)]
          )

test_parse :: Bool
test_parse = ((parseString $ fst exampleStrings) == fst example) && ((parseString $ snd exampleStrings) == snd example)
      

-- Calculations
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
    
getOverlaps :: ([Position], [Position]) -> [Position]
getOverlaps (p1, p2) =
  let 
    set1 = Set.fromList p1
    set2 = Set.fromList p2
  in
    Set.toList $ Set.intersection set1 set2

-- Part 1
getMinimumOverlap :: ([Position], [Position]) -> Int
getMinimumOverlap ps =
  let 
    overlaps = getOverlaps ps
    dists = map calcdist overlaps
      where calcdist (a, b) = abs a + abs b
  in
    minimum dists

getMinimum :: ([Movement],[Movement]) -> Int
getMinimum (m1, m2) = 
  getMinimumOverlap (getAllPositions startingPosition m1, getAllPositions startingPosition m2)


-- examples Part 1 & Part 2
examples :: [(String,String,Int,Int)] -- Wire 1, Wire 2, Result Part 1, Result Part 2
examples = [
  ("R8,U5,L5,D3","U7,R6,D4,L4",6,30),
  ("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83",159,610),
  ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",135,410)
  ]

check_examples_part1 :: [(String,String,Int,Int)] -> [Bool]
check_examples_part1 = map check_example_part1 

check_example_part1 :: (String,String,Int,Int) -> Bool
check_example_part1 (s1, s2, r, _) =
  let 
    m1 = parseString s1
    m2 = parseString s2
  in 
    getMinimum (m1,m2) == r


check_examples_part2 :: [(String,String,Int,Int)] -> [Bool]
check_examples_part2 = map check_example_part2 

check_example_part2 :: (String,String,Int,Int) -> Bool
check_example_part2 (s1, s2, _, r) =
  let 
    m1 = parseString s1
    m2 = parseString s2
  in 
    getMinimumSteps m1 m2 == r

-- Part 2
addSteps:: [Position] -> [(Position, Int)]
addSteps pos = 
  zip pos [1..]

getStepsForIntersection :: [Position] -> [(Position, Int)] -> [(Position, Int)]
getStepsForIntersection intersections positions =
  filter (\(p, s) -> p `elem` intersections) positions


combineSteps :: [(Position, Int)] -> [(Position, Int)] -> [(Position, Int)]
combineSteps a b =
  map (\(pos, dist1) -> (pos, dist1 + (getDist pos b)) ) a
  where getDist pos list = snd $ Data.Maybe.fromJust $ Data.List.find (\(p,_) -> p == pos) list

getMinimumSteps' :: [(Position, Int)] -> Int
getMinimumSteps' list =
  let 
    (p,s) = unzip list
  in 
    minimum s

getMinimumSteps :: [Movement] -> [Movement] -> Int
getMinimumSteps m1 m2 =
    let
      pos1 = getAllPositions startingPosition m1
      pos2 = getAllPositions startingPosition m2
      intersections = getOverlaps (pos1, pos2)
      steps1 = getStepsForIntersection intersections $ addSteps pos1
      steps2 = getStepsForIntersection intersections $ addSteps pos2
    in 
      getMinimumSteps' $ combineSteps steps1 steps2


main = do
  contents <- readFile "input"
  let ms = map parseString $ lines $ contents
  let m1 = head ms
  let m2 = last ms
  print $ getMinimumSteps m1 m2


