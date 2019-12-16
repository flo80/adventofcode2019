import Data.List (nub, maximumBy, groupBy, sortBy, transpose, concat, (\\), all)

main = do
  contents <- readFile "input"
  putStr "Part1: "
  print $ part1 $ contents
  putStr "Part2: "
  print $ part2 $ contents

type Point = (Int, Int)
  
parseMap :: String -> [Point]
parseMap contents =  foldl (\acc (rowNr, rowString) -> acc ++ gatherAsteroids(rowNr, rowString)) [] $ zip  [0..] $ lines contents
  where
    gatherAsteroids (rowNr, rowString) = snd $ foldl (\(colNr, acc) c -> (colNr + 1, acc ++ newEntry (colNr,rowNr) c)) (0,[]) rowString 
    newEntry p '.' = []
    newEntry p '#' = [p]


-- Part 1

part1 :: String -> Int
part1 contents = snd $ selectedAsteroid asteroids
  where 
    asteroids = parseMap contents

-- upwards 0, up to 2 pi clockwise
calcAngles :: Point -> [Point] -> [Double]
calcAngles center list = map (getAngle center) list
  where 
    getAngle (x1,y1) (x2,y2) = if at < 0 then at + 2*pi else at
      where
        dx = fromIntegral $ negate $ x2 - x1
        dy = fromIntegral $ negate $ y2 - y1
        at = atan2 dy dx - pi / 2.0 


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

selectedAsteroid :: [Point] -> (Point, Int)
selectedAsteroid asteroids = maximumBy(\a b -> compare (snd a) (snd b)) $ zip asteroids $ map inView asteroids
  where 
    inView a = (length $ nub $ calcAngles a $ filter (/= a) asteroids) 


-- Part 2 

part2 :: String -> Int
part2 contents = fm (shootingList asteroids !! 199)
  where 
    asteroids = parseMap contents
    fm a = fst a * 100 + snd a

type AngleGroup = (Double, [Point])

shootingList :: [Point] -> [Point]
shootingList asteroids = concat $ transpose $ map snd $ createGroups asteroids

createGroups :: [Point] -> [AngleGroup]
createGroups input = sortBy (\a b -> compare (fst a) (fst b)) $ groups
  where
    laser = fst $ selectedAsteroid input
    asteroids = input \\ [laser]
    angles = groupBy (\a b -> snd a == snd b) $ sortBy (\a b -> compare (snd a) (snd b)) $ zip asteroids $ calcAngles laser asteroids
    groups = map (\i -> (angle i, items i)) angles
      where 
        angle = snd . head
        dist (x,y)  = sqrt $ (fromIntegral(x - fst laser) ** 2 + fromIntegral(y - snd laser) ** 2) :: Double
        items = sortBy (\a b -> compare (dist a) (dist b)) . map fst  

examples2 :: [(Int,Int)]
examples2 = [(1,1112), (2,1201), (3,1202), (10,1208), (20,1600), (50,1609),(100,1016),(199,906),(200,802),(201,1009),(299,1101)]

testExamples2 :: Bool
testExamples2 = all (== True) $ map check examples2
  where
      asteroids = parseMap $ fst $ last examples1
      sl = shootingList asteroids
      fm a = fst a * 100 + snd a
      check (pos,res) = fm (sl!!(pos-1)) == res :: Bool
      
testPart2 :: Bool
testPart2 = part2 (fst $ last examples1) == 802