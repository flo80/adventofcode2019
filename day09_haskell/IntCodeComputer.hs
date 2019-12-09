{-# LANGUAGE RecordWildCards #-}

module IntCodeComputer
  ( Computer(..)
  , Memory
  , Value
  , State(..)
  , newComputer
  , interactiveComputer
  , runComputer
  , resetOutput
  , addInput
  , test_code
  )
where

import           Data.Foldable                            ( toList )
import           Data.Sequence                            ( Seq
                                                          , fromList
                                                          , lookup
                                                          , update
                                                          , (><)
                                                          , replicate
                                                          )
import           Data.List.Split                          ( splitOn )
import           Data.List                                ( repeat )
import           Data.Maybe                               ( fromMaybe )


data Computer = Computer {
  memory :: Memory,
  ip :: Pointer,
  relativeBase :: Pointer,
  state :: State,
  input :: [Value],
  output :: [Value]
} deriving (Show)

type Memory = (Seq Value)
type Value = Int
type Pointer = Int
data State = Running | Halted | Crashed | WaitingForInput
  deriving (Show, Eq)

data Instruction
  = InstrAdd Parameter Parameter Parameter
  | InstrMult Parameter Parameter Parameter
  | InstrInput Parameter
  | InstrOutput Parameter
  | JmpTrue Parameter Parameter
  | JmpFalse Parameter Parameter
  | CmpLT Parameter Parameter Parameter
  | CmpEQ Parameter Parameter Parameter
  | InstrSetBase Parameter
  | InstrHalt
  | InstrInvalid Int

data Parameter
  = PositionMode Pointer
  | ImmediateMode Value
  | RelativeMode Pointer
  | InvalidMode Int
  deriving (Show)


type Process = Computer -> Computer


-- Creation of computer

newComputer :: String -> [Value] -> Computer
newComputer program input = Computer { memory       = parseProgram program
                                     , ip           = 0
                                     , relativeBase = 0
                                     , state        = Running
                                     , input        = input
                                     , output       = []
                                     }

parseProgram :: String -> Memory
parseProgram contents = fromList $ map read $ splitOn "," contents


-- Helper to deal with output                                    

resetOutput :: Computer -> (Computer, [Value])
resetOutput computer =
  let o = output computer
      c = computer { output = [] }
  in  (c, o)

addInput :: Computer -> [Int] -> Computer
addInput computer new =
  let old = input computer in computer { input = old ++ new, state = Running }


-- Run computer

interactiveComputer :: String -> [Int] -> [Int]
interactiveComputer code inputs =
  let c = newComputer code inputs in output $ runComputer c

runComputer :: Process
runComputer computer@Computer { state = s }
  | s == Running = runComputer $ processInstruction computer $ fetchInstruction
    computer
  | s == WaitingForInput = computer
  | s == Halted = computer
  | s == Crashed = computer


-- Process Instructions

fetchInstruction :: Computer -> Instruction
fetchInstruction computer =
  let ip'    = ip computer
      opCode = getMemory computer ip'

      (instrCode, parameterModes) =
          let instrCode = opCode `mod` 100
              parameterModes =
                  (reverse $ toDigits $ opCode `div` 100) ++ (Data.List.repeat 0)  -- 0 is default parameter mode
          in  (instrCode, parameterModes) :: (Value, [Int])

      mp nr =
          let mode = parameterModes !! (nr - 1)
              val  = getMemory computer (ip' + nr)
          in  case mode of
                0 -> PositionMode val
                1 -> ImmediateMode val
                2 -> RelativeMode val
                _ -> InvalidMode mode
  in  case instrCode of
        1  -> InstrAdd (mp 1) (mp 2) (mp 3)
        2  -> InstrMult (mp 1) (mp 2) (mp 3)
        3  -> InstrInput (mp 1)
        4  -> InstrOutput (mp 1)
        5  -> JmpTrue (mp 1) (mp 2)
        6  -> JmpFalse (mp 1) (mp 2)
        7  -> CmpLT (mp 1) (mp 2) (mp 3)
        8  -> CmpEQ (mp 1) (mp 2) (mp 3)
        9  -> InstrSetBase (mp 1)
        99 -> InstrHalt
        _  -> InstrInvalid instrCode


processInstruction :: Computer -> Instruction -> Computer
processInstruction computer (InstrAdd noun verb dest) =
  processMath (+) noun verb dest computer
processInstruction computer (InstrMult noun verb dest) =
  processMath (*) noun verb dest computer
processInstruction computer (InstrInput dest)
  | (length $ input computer) == 0
  = computer { state = WaitingForInput }
  | otherwise
  = let value    = head $ input computer
        newInput = tail $ input computer
    in  writeM computer { input = newInput, ip = ip computer + 2 } dest value
processInstruction computer (InstrOutput val) =
  let value     = readM computer val
      newOutput = (output computer) ++ [value]
  in  computer { output = newOutput, ip = ip computer + 2 }
processInstruction computer (JmpTrue cmp dest) =
  processJmp (/= 0) cmp dest computer
processInstruction computer (JmpFalse cmp dest) =
  processJmp (== 0) cmp dest computer
processInstruction computer (CmpLT cmpA cmpB dest) =
  processCmp (<) cmpA cmpB dest computer
processInstruction computer (CmpEQ cmpA cmpB dest) =
  processCmp (==) cmpA cmpB dest computer
processInstruction computer (InstrSetBase val) =
  let value   = readM computer val
      newBase = relativeBase computer + value
  in  computer { relativeBase = newBase, ip = ip computer + 2 }
processInstruction computer (InstrHalt) = computer { state = Halted }
processInstruction computer (InstrInvalid code) =
  error
    $  "Invalid Instruction code "
    ++ show code
    ++ " at "
    ++ (show $ ip computer)


processMath
  :: (Value -> Value -> Value)
  -> Parameter
  -> Parameter
  -> Parameter
  -> Computer
  -> Computer
processMath ops noun verb dest computer =
  let result = ops (readM computer noun) (readM computer verb)
  in  writeM computer { ip = ip computer + 4 } dest result

processJmp :: (Value -> Bool) -> Parameter -> Parameter -> Computer -> Computer
processJmp ops cmp dest computer =
  let destLocation = case ops (readM computer cmp) of
        True  -> readM computer dest
        False -> ip computer + 3
  in  computer { ip = destLocation }

processCmp
  :: (Value -> Value -> Bool)
  -> Parameter
  -> Parameter
  -> Parameter
  -> Computer
  -> Computer
processCmp ops cmpA cmpB dest computer =
  let result = case ops (readM computer cmpA) (readM computer cmpB) of
        True  -> 1
        False -> 0
  in  writeM (computer { ip = ip computer + 4 }) dest result


-- Memory operations

readM :: Computer -> Parameter -> Value
readM computer (PositionMode  pointer) = getMemory computer pointer
readM computer (ImmediateMode value  ) = value
readM computer (RelativeMode pointer) =
  getMemory computer (pointer + relativeBase computer)
readM computer (InvalidMode m) =
  error $ "Invalid address mode for parameter " ++ show m

getMemory :: Computer -> Pointer -> Value
getMemory computer pointer
  | pointer < 0 = error "Negative address for memory read"
  | otherwise   = fromMaybe 0 $ Data.Sequence.lookup pointer (memory computer)


writeM :: Computer -> Parameter -> Value -> Computer
writeM computer dest value =
  let newMemory =
          updateMemory (getDestination computer dest) value (memory computer)
  in  computer { memory = newMemory }

getDestination :: Computer -> Parameter -> Pointer
getDestination computer (PositionMode pointer) = pointer
getDestination computer (RelativeMode pointer) =
  pointer + relativeBase computer
getDestination _ p = error $ "Invalid address mode for destination: " ++ show p

updateMemory :: Pointer -> Value -> Memory -> Memory
updateMemory dest value memory
  | dest < 0
  = error "Negative address for memory write"
  | otherwise
  = let longMemory = case dest < (length memory) of
          True -> memory
          False ->
            memory >< Data.Sequence.replicate (dest - (length memory) + 1) 0
    in  update dest value longMemory


-- Helper methods
toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: Integral x => [x] -> x
fromDigits = foldl addDigit 0 where addDigit num d = 10 * num + d



-- ((Memory, Input),(Memory, Output))
type ExampleOutput = ([Int], [Int])

examplesWithCode :: [((String, [Int]), ExampleOutput)]
examplesWithCode =
  [ (("1,0,0,0,99", [])         , ([2, 0, 0, 0, 99], []))
  , (("2,3,0,3,99", [])         , ([2, 3, 0, 6, 99], []))
  , (("2,4,4,5,99,0", [])       , ([2, 4, 4, 5, 99, 9801], []))
  , (("1,1,1,4,99,5,6,0,99", []), ([30, 1, 1, 4, 2, 5, 6, 0, 99], []))
  , ( ("1,9,10,3,2,3,11,0,99,30,40,50"               , [])
    , ([3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50], [])
    )
  ,
  -- input output test
    (("3,0,4,0,99", [0])    , ([0, 0, 4, 0, 99], [0]))
  , (("3,0,4,0,99", [-1])   , ([-1, 0, 4, 0, 99], [-1]))
  ,
  -- test rewriting own code
    (("1101,100,-1,4,0", []), ([1101, 100, -1, 4, 99], []))
  ]

testExamplesWithCode
  :: [((String, [Int]), ExampleOutput)]
  -> [(Bool, ExampleOutput, ExampleOutput)]
testExamplesWithCode examples =
  let (cases, expected) = Prelude.unzip examples
      results =
          map (\c -> (toList $ memory c, output c)) $ map runComputer $ map
            (\(code, input) -> newComputer code input)
            cases
      checks = map (\(e, r) -> e == r) $ Prelude.zip expected results :: [Bool]
  in  Prelude.zip3 checks expected results

-- Program, Input, Output
examplesIO :: [(String, [Int], [Int])]
examplesIO =
  -- position mode - eq 8
  [ ("3,9,8,9,10,9,4,9,99,-1,8", [8], [1])
  , ("3,9,8,9,10,9,4,9,99,-1,8", [7], [0])
  , ( "3,9,8,9,10,9,4,9,99,-1,8"
    , [9]
    , [0]
    )
  -- position mode - lt 8
  , ("3,9,7,9,10,9,4,9,99,-1,8", [9], [0])
  , ("3,9,7,9,10,9,4,9,99,-1,8", [8], [0])
  , ( "3,9,7,9,10,9,4,9,99,-1,8"
    , [7]
    , [1]
    )
  -- immediate mode - eq 8
  , ("3,3,1108,-1,8,3,4,3,99", [8], [1])
  , ("3,3,1108,-1,8,3,4,3,99", [7], [0])
  , ( "3,3,1108,-1,8,3,4,3,99"
    , [9]
    , [0]
    )
  -- immediate mode - lt 8
  , ("3,3,1107,-1,8,3,4,3,99", [9], [0])
  , ("3,3,1107,-1,8,3,4,3,99", [8], [0])
  , ( "3,3,1107,-1,8,3,4,3,99"
    , [7]
    , [1]
    )
  -- position mode - jump if zero
  , ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", [0], [0])
  , ( "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
    , [5]
    , [1]
    )
  -- immediate mode - jump if zero
  , ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", [0], [0])
  , ( "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
    , [5]
    , [1]
    )
  -- check if input is 8
  , ( "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    , [7]
    , [0999]
    )
  , ( "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    , [8]
    , [1000]
    )
  , ( "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    , [9]
    , [1001]
    )
  , ( "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    , []
    , [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
    )
  , ("104,1125899906842624,99"          , [], [1125899906842624])
  , ("1102,34915192,34915192,7,4,7,99,0", [], [1219070632396864])
  ]


testExamplesIO :: [(String, [Value], [Value])] -> [(Bool, [Value], [Value])]
testExamplesIO examples =
  let (code, input, expectedOutput) = unzip3 examples
      cases                         = zip code input
      results                       = map output $ map runComputer $ map
        (\(code, input) -> newComputer code input)
        cases
      checks = map (\(e, r) -> e == r) $ zip expectedOutput results
  in  zip3 checks expectedOutput results

test_code :: Bool
test_code =
  let (checks  , _, _) = Prelude.unzip3 $ testExamplesWithCode examplesWithCode
      (checksIO, _, _) = Prelude.unzip3 $ testExamplesIO examplesIO
  in  all (== True) checks && all (== True) checksIO

