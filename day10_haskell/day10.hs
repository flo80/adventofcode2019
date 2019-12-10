import Data.List (nub)

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  -- putStr "Part2: "
  -- print $ part2 $ contents

type Point = (Int, Int)
  
parseMap :: String -> [Point]
parseMap contents =  foldl (\acc (rowNr, rowString) -> acc ++ gatherAsteroids(rowNr, rowString)) [] $ zip  [0..] $ lines contents
  where
    gatherAsteroids (rowNr, rowString) = snd $ foldl (\(colNr, acc) c -> (colNr + 1, acc ++ newEntry (colNr,rowNr) c)) (0,[]) rowString 
    newEntry p '.' = []
    newEntry p '#' = [p]

    
-- Part 1

part1 :: String -> Int
part1 contents = maximum $ map inView asteroids
  where 
    asteroids = parseMap contents
    inView a = (length $ nub $ calcAngles a asteroids) 

calcAngles :: Point -> [Point] -> [Double]
calcAngles center list = map (getAngle center) list
  where 
    getAngle (x1,y1) (x2,y2) = atan2 dy dx
      where
        dx = fromIntegral $ x2 - x1
        dy = fromIntegral $ negate $ y2 - y1


examples1 :: [(String, Int)]
examples1 = 
  [(".#..#\n.....\n#####\n....#\n...##\n",8)
  ,("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####\n", 33)
  ,("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.\n",35)
  ,(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..\n",41)
  ,(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n",210)
  ]

testExamples1 :: Bool
testExamples1 = (map (part1 . fst) examples1) == (map snd examples1)
