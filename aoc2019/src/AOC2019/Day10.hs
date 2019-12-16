module AOC2019.Day10
  ( day10a
  , day10b
  , day10run
  )
where

import           Data.List                                ( nub
                                                          , maximumBy
                                                          , groupBy
                                                          , sortBy
                                                          , transpose
                                                          , concat
                                                          , (\\)
                                                          , all
                                                          )


day10run :: IO ()
day10run = do
  contents <- readFile "input/day10"
  putStr "Day 10 - Part 1: "
  print $ day10a contents
  putStr "Day 10 - Part 2: "
  print $ day10b contents
  putStrLn ""


type Point = (Int, Int)

parseMap :: String -> [Point]
parseMap contents =
  foldl (\acc (rowNr, rowString) -> acc ++ gatherAsteroids (rowNr, rowString))
        []
    $ zip [0 ..]
    $ lines contents
 where
  gatherAsteroids (rowNr, rowString) = snd $ foldl
    (\(colNr, acc) c -> (colNr + 1, acc ++ newEntry (colNr, rowNr) c))
    (0, [])
    rowString
  newEntry p '.' = []
  newEntry p '#' = [p]

day10a :: String -> Int
day10a = snd . selectedAsteroid . parseMap

day10b :: String -> Int
day10b contents = fm (shootingList asteroids !! 199)
 where
  asteroids = parseMap contents
  fm a = fst a * 100 + snd a

-- Part 1

selectedAsteroid :: [Point] -> (Point, Int)
selectedAsteroid asteroids =
  maximumBy (\a b -> compare (snd a) (snd b)) $ zip asteroids $ map inView
                                                                    asteroids
  where inView a = (length $ nub $ calcAngles a asteroids)


-- upwards 0, up to 2 pi clockwise
calcAngles :: Point -> [Point] -> [Double]
calcAngles center list = map (getAngle center) list
 where
  getAngle (x1, y1) (x2, y2) = if at < 0 then at + 2 * pi else at
   where
    dx = fromIntegral $ negate $ x2 - x1
    dy = fromIntegral $ negate $ y2 - y1
    at = atan2 dy dx - pi / 2.0


-- Part 2

type AngleGroup = (Double, [Point])

shootingList :: [Point] -> [Point]
shootingList asteroids = concat $ transpose $ map snd $ createGroups asteroids

createGroups :: [Point] -> [AngleGroup]
createGroups input = sortBy (\a b -> compare (fst a) (fst b)) $ groups
 where
  laser     = fst $ selectedAsteroid input
  asteroids = input \\ [laser]
  angles =
    groupBy (\a b -> snd a == snd b)
      $ sortBy (\a b -> compare (snd a) (snd b))
      $ zip asteroids
      $ calcAngles laser asteroids
  groups = map (\i -> (angle i, items i)) angles
   where
    angle = snd . head
    dist (x, y) =
      sqrt
        $ (fromIntegral (x - fst laser) ** 2 + fromIntegral (y - snd laser) ** 2
          ) :: Double
    items = sortBy (\a b -> compare (dist a) (dist b)) . map fst

