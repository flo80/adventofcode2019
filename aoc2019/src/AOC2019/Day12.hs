module AOC2019.Day12
  ( day12a
  , day12b
  , day12run
  )
where

-- cool solution: https://github.com/ephemient/aoc2019/blob/hs/src/Day12.hs

import           Linear.V3  
import           Data.Maybe                               ( fromJust ) 
import           Data.List                                ( tails, elemIndex )
import           Data.List.Split                          ( splitOn )

day12run :: IO ()
day12run = do
  contents <- readFile "input/day12"
  putStr "Day 12 - Part 1: "
  print $ day12a (contents, 1000)
  putStr "Day 12 - Part 2: "
  print $ day12b contents
  putStrLn ""


type Pos = V3 Int
zeroPos :: Pos
zeroPos = V3 0 0 0

data Moon = Moon
  { position:: Pos
  , velocity :: Pos
  } deriving (Eq, Ord)
instance Show Moon where
  show m =
    "pos=" ++ showV (position m) ++ ", vel=" ++ showV (velocity m) ++ "\n"
   where
    showV (V3 x y z) =
      "<x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z ++ ">"

      
parseInput :: String -> [Moon]
parseInput = map (moon . parseLine) . lines
 where
  moon [x, y, z] = Moon (V3 x y z) zeroPos
  parseLine :: String -> [Int]
  parseLine = map (read . nr) . tokens . drop 1 . reverse . drop 1 . reverse
  nr        = last . splitOn "="
  tokens    = splitOn ", "


day12a :: (String, Int) -> Int
day12a (contents, iterations) = totalEnergy $ last $ take iterations $ drop 1 $ iterate step $ parseInput contents
  where
    totalEnergy :: [Moon] -> Int
    totalEnergy = sum . map energy
      where energy (Moon p v) = (sum $ abs p) *  (sum $ abs v)


step :: [Moon] -> [Moon]
step ms = map (applyVelocity . applyGravity ms) ms
  where 
    applyVelocity :: Moon -> Moon
    applyVelocity (Moon pos vel) = Moon (pos + vel) vel

    applyGravity :: [Moon] -> Moon -> Moon
    applyGravity ms m = m { velocity = velocity m + update m }
      where
        update :: Moon -> Pos
        update m = sum $ map (g m) ms

        -- gets the change to velocity vector for m1
        g :: Moon -> Moon -> Pos
        g m1 m2 = signum (position m2 - position m1)


-- simplified based on reddit comments (no saving all states in set but just look for first state again)        
day12b :: String -> Int
day12b contents = lcm zLoop $ lcm xLoop yLoop
 where
  iterations :: [[Moon]]
  iterations = iterate step $ parseInput contents
  xs (Moon (V3 px py pz) (V3 vx vy vz)) = (px, vx)
  ys (Moon (V3 px py pz) (V3 vx vy vz)) = (py, vy)
  zs (Moon (V3 px py pz) (V3 vx vy vz)) = (pz, vz)
  xLoop = succ $ fromJust $ elemIndex (map xs $ head iterations)  $ map (map xs) $ drop 1 iterations
  yLoop = succ $ fromJust $ elemIndex (map ys $ head iterations)  $ map (map ys) $ drop 1 iterations
  zLoop = succ $ fromJust $ elemIndex (map zs $ head iterations)  $ map (map zs) $ drop 1 iterations
