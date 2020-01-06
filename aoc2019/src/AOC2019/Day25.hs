module AOC2019.Day25
  ( day25a
  , day25a_interactive
  , day25b
  , day25run
  )
where

import           Debug.Trace
import           AOC2019.IntCodeComputer
import           Text.ParserCombinators.ReadP
import           Data.Char                                ( toLower
                                                          , isNumber
                                                          )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import qualified Control.Monad.State           as MS
import           Data.List                                ( all
                                                          , any
                                                          , find
                                                          , (\\)
                                                          , isInfixOf
                                                          )
import           Data.Maybe                               ( catMaybes
                                                          , fromJust
                                                          )

day25run :: IO ()
day25run = do
  contents <- readFile "input/day25"
  putStr "Day 25 - Part 1: "
  putStr $ day25a contents
  -- putStr "Day 25 - Part 2: "
  -- print $ day25b contents
  putStrLn ""


day25a :: String -> String
day25a contents = filter isNumber lastline
 where
  finalC   = enterRoom $ prepareForSecurity $ newComputer contents []
  result   = showOutput $ output $ runComputer finalC
  lastline = last $ lines result

showOutput :: [Int] -> String
showOutput = concatMap showChar
 where
  showChar :: Int -> String
  showChar c | c > 255   = show $ c
             | otherwise = [toEnum c]

encodeInput :: String -> [Int]
encodeInput x = map (fromEnum . toLower) (x ++ "\n")

day25a_interactive :: IO ()
day25a_interactive = do
  contents <- readFile "input/day25"
  let computer = newComputer contents []
  -- clearScreen
  -- setCursorPosition 0 0
  loop_interactive computer


loop_interactive :: Computer -> IO ()
loop_interactive c = do
  let (nc, o) = resetOutput $ runComputer c
  let output  = showOutput o
  putStr output

  newIn <- getLine

  let nc2 = addInput nc $ encodeInput newIn
  loop_interactive nc2


enterRoom :: (Computer, [String], Direction) -> Computer
enterRoom (computer, allItems, direction) =
  addInput computer
    $ encodeInput
    $ (  (concatMap (\x -> "take " ++ x ++ "\n") workingCombination)
      ++ show direction
      )
 where
  workingCombination =
    snd $ head $ testCombination' (allItems, direction) (computer, [])

  testCombination'
    :: ([String], Direction) -> (Computer, [String]) -> [(Computer, [String])]
  testCombination' (allItems, direction) (computer, inHand) = concatMap
    test
    possibleItems
   where
    possibleItems = allItems \\ inHand

    test :: String -> [(Computer, [String])]
    test item = case tooHeavy o of
      True  -> []
      False -> case tooLight o of
        True  -> testCombination' (allItems, direction) (nc, item : inHand)
        False -> [(nc, item : inHand)]
     where
      tooLight = isInfixOf "are heavier"
      tooHeavy = isInfixOf "are lighter"
      (nc, o)  = try item

      try item = (nc, showOutput o)
       where
        (nc, o) = resetOutput $ runComputer $ addInput computer $ encodeInput
          ("take " ++ item ++ "\n" ++ show direction)



-- collect all relevant items and walk to room before check
-- gets Computer for this, list of items and direction towards check
prepareForSecurity :: Computer -> (Computer, [String], Direction)
prepareForSecurity computer =
  (goToTargetRoom $ getAllItems computer, possibleItems, nextStep)
 where
  testRoomName   = "Pressure-Sensitive Floor"
  targetRoomName = "Security Checkpoint"
  rooms          = findAllRooms computer
  possibleItems  = (concatMap items rooms) \\ forbiddenItems
  forbiddenItems =
    [ "infinite loop"
    , "escape pod"
    , "molten lava"
    , "photons"
    , "giant electromagnet"
    ]
  nextStep = head $ way $ fromJust $ find (\x -> name x == testRoomName) rooms

  goToTargetRoom :: Computer -> Computer
  goToTargetRoom computer = nc
   where
    (nc, o) = resetOutput $ runComputer $ addInput computer $ encodeInput
      (instrsWalk ++ instrsDrop)
    instrsWalk = concatMap (\x -> show x ++ "\n") $ reverse $ way room
    instrsDrop = concatMap (\x -> "drop " ++ x ++ "\n") possibleItems
    room       = fromJust $ find (\x -> name x == targetRoomName) rooms


  getAllItems :: Computer -> Computer
  getAllItems computer = foldl collectItem computer roomsWithItem
   where
    collectItem :: Computer -> Room -> Computer
    collectItem computer room = runComputer $ addInput computer $ encodeInput
      fullInstr
     where
      instrsWalk = concatMap (\x -> show x ++ "\n") $ reverse $ way room
      instrsTake = concatMap (\i -> "take " ++ i ++ "\n") $ items room
      instrsBack = concatMap (\x -> show (opposite x) ++ "\n") $ way room
      fullInstr  = instrsWalk ++ instrsTake ++ instrsBack

    roomsWithItem =
      filter (\r -> any (== False) $ map (`elem` forbiddenItems) (items r))
        $ filter (\r -> length (items r) > 0)
        $ rooms


findAllRooms :: Computer -> [Room]
findAllRooms computer = Map.elems $ snd $ MS.runState
  (findAllRooms' [] computer)
  Map.empty
 where
  findAllRooms' :: [Direction] -> Computer -> MS.State (Map String Room) ()
  findAllRooms' wayuptohere c = do
    rooms <- MS.get
    let (nc, o) = resetOutput $ runComputer c
    let r'      = parseOutputIntoRoom $ showOutput o

    case r' of
      Nothing    -> return ()
      Just room' -> do
        let room     = room' { way = wayuptohere }
        let newRooms = Map.insert (name room) room rooms

        case Map.member (name room) rooms of
          True  -> return ()
          False -> do
            MS.put newRooms
            mapM_
                (\dir ->
                  findAllRooms' (dir : wayuptohere)
                    $ addInput nc
                    $ encodeInput
                    $ show dir
                )
              $ doors room



-- Functions to parse output into data structure

data Room = Room
  { name  :: String
  , doors :: [Direction]
  , items :: [String]
  , way   :: [Direction]
  } deriving (Show)

data Direction = North | East | South | West deriving (Show)
opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East


parseOutputIntoRoom :: String -> Maybe Room
parseOutputIntoRoom output = if length parsed > 0
  then Just (fst $ head $ parsed)
  else Nothing
 where
  parsed = readP_to_S roomP output

  roomP :: ReadP Room
  roomP = do
    name <- roomNameP
    skipMany1 (satisfy (\_ -> True))
    doors <- directionsP
    items <- option [] itemsP
    _     <- string "Command?\n"
    eof
    return (Room name doors items [])

  roomNameP :: ReadP String
  roomNameP = do
    skipMany $ string "\n"
    between (string "== ") (string " ==\n") $ many1 (satisfy (\_ -> True))

  directionsP :: ReadP [Direction]
  directionsP = do
    _ <- string "Doors here lead:\n"
    manyTill (choice [east, west, north, south]) $ string "\n"
   where
    east  = string "- east\n" >> return East
    west  = string "- west\n" >> return West
    north = string "- north\n" >> return North
    south = string "- south\n" >> return South

  itemsP :: ReadP [String]
  itemsP = do
    _ <- string "Items here:\n"
    manyTill oneItem $ string "\n"
   where
    oneItem =
      between (string "- ") (string "\n") $ many1 (satisfy (\_ -> True))


day25b :: String -> Int
day25b = undefined
