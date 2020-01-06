module AOC2019.Day23
  ( day23a
  , day23b
  , day23run
  )
where

import           Data.List                                ( concat )
import           AOC2019.IntCodeComputer
import           Data.Maybe                               ( fromJust )
import           Debug.Trace

day23run :: IO ()
day23run = do
  contents <- readFile "input/day23"
  putStr "Day 23 - Part 1: "
  print $ day23a contents
  putStr "Day 23 - Part 2: "
  print $ day23b contents
  putStrLn ""


day23a :: String -> Int
day23a contents = loop computers
  where computers = map (\id -> newComputer contents [id]) [0 .. 49]

getOutputs :: [Computer] -> ([Computer], [[Int]])
getOutputs = unzip . map (resetOutput . runComputer)

grouping :: [Int] -> [(Int, Int, Int)]
grouping []               = []
grouping (a : b : c : xs) = (a, b, c) : grouping xs

getAllInputs :: [[Int]] -> [[Int]]
getAllInputs inputs =
  map (getInputForId (grouping $ concat inputs)) ([0 .. 49] ++ [255])

getInputForId :: [(Int, Int, Int)] -> Int -> [Int]
getInputForId inputs id = case null inp of
  True  -> [-1]
  False -> inp
 where
  inp = foldl (\a (t, x, y) -> x : y : a) []
    $ filter (\(tgt, x, y) -> tgt == id) inputs

addInp :: [[Int]] -> [Computer] -> [Computer]
addInp inputs computers =
  map (\(c, i) -> addInput c i) $ zip computers $ init inputs

loop :: [Computer] -> Int
loop computers = case (head $ last allInputs) > (-1) of
  True  -> last $ last allInputs
  False -> loop $ addInp allInputs nc
 where
  (nc, inputsTemp) = getOutputs computers
  allInputs        = getAllInputs inputsTemp


day23b :: String -> Int
day23b contents = loop2 Nothing Nothing computers
  where computers = map (\id -> newComputer contents [id]) $ [0 .. 49]


-- this can probably be simplified - but it works
loop2 :: Maybe (Int, Int) -> Maybe Int -> [Computer] -> Int
loop2 prevLastReceived prevY computers = case prevY of
  Nothing -> loop2 newLastReceived newY nc2
  Just yy -> case newY of
    Nothing    -> loop2 newLastReceived newY nc2
    Just newYY -> case yy == newYY of
      False -> loop2 newLastReceived newY nc2
      True  -> newYY
 where
  (nc, inputsTemp) = getFirstOutput computers
  inputs           = grouping $ concat inputsTemp
  empty =
    (  ([] == concat inputsTemp)
    && ([] == concatMap input nc)
    && (all (== WaitingForInput) $ map state nc)
    )

  --nat business logic
  (newLastReceived, msg) =
    nat empty (getInputForId inputs 255) prevLastReceived
  (newInputs, newY) = case msg of
    Nothing        -> (inputs, Nothing)
    Just (a, b, c) -> ((a, b, c) : inputs, Just c)

  nc2 = addInp' newInputs nc


addInp' :: [(Int, Int, Int)] -> [Computer] -> [Computer]
addInp' allInputs computers = map addI $ zip [0 ..] computers
 where
  addI (idx, c) = case length inp > 0 of
    True  -> addInput c inp
    False -> case state c of
      WaitingForInput -> addInput c [-1]
      _               -> c
    where inp = getInputForId allInputs idx


-- need to not run all computers in parallel until they wait for input but iteratively collect outputs and distribute them
getFirstOutput :: [Computer] -> ([Computer], [[Int]])
getFirstOutput = unzip . step
 where
  step :: [Computer] -> [(Computer, [Int])]
  step cs = case any (== 3) $ map (length . output) ncs of
    False -> case all (== WaitingForInput) $ map state ncs of
      True  -> map (\c -> (c, [])) ncs
      False -> step ncs
    True -> map getO ncs
   where
    ncs = map stepComputer cs
    getO c = case length $ output c of
      3 -> resetOutput c
      _ -> (c, [])


nat
  :: Bool
  -> [Int]
  -> Maybe (Int, Int)
  -> (Maybe (Int, Int), Maybe (Int, Int, Int))
nat allInputsEmpty msg lastReceived = case allInputsEmpty of
  True -> case lastReceived of
    Nothing     -> (lastReceived, Nothing)
    Just (x, y) -> (lastReceived, Just (0, x, y)) -- send out package

  False -> case msg of
    [-1       ] -> (lastReceived, Nothing) -- no new package for nat
    (a : b : _) -> (Just (a, b), Nothing) -- new package for nat
