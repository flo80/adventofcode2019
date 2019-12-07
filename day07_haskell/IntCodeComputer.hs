module IntCodeComputer
  ( Computer
  , Memory
  , State(..)
  , parseProgram
  , newComputer
  , runComputer
  , getMemory
  , getOutput
  , state
  , test_code
  )
where

import           Data.Foldable                            ( toList )
import           Data.Sequence                            ( Seq
                                                          , fromList
                                                          , index
                                                          , update
                                                          )
import           Data.List.Split                          ( splitOn )
import           Data.List                                ( repeat )


data Computer = Computer {
  memory :: Memory,
  state :: State,
  ip :: Pointer,
  input :: [Int],
  output :: [Int]
} deriving (Show)

type Memory = (Seq Int)
data State = Running | Halted | Crashed
  deriving (Show, Eq)
type Pointer = Int

parseProgram :: String -> Memory
parseProgram contents = fromList $ map read $ splitOn "," contents

newComputer :: Memory -> [Int] -> Computer
newComputer memory input = Computer { memory = memory
                                    , state  = Running
                                    , ip     = 0
                                    , input  = input
                                    , output = []
                                    }

getMemory :: Computer -> [Int]
getMemory Computer { memory = m } = toList m

getOutput :: Computer -> [Int]
getOutput Computer { output = o } = o

runComputer :: Computer -> Computer
runComputer computer@Computer { state = s }
  | s == Running = runComputer $ processInstruction computer
  | s == Halted  = computer
  | s == Crashed = computer

processInstruction :: Computer -> Computer
processInstruction computer@Computer { state = Halted } = computer
processInstruction computer@Computer { memory = memory, ip = pointer, input = input, output = output }
  = let i@(Instruction instruction parameters) =
            decodeInstruction memory pointer
        newPointer = case instruction of
          JmpTrue  -> process_jmp memory i pointer
          JmpFalse -> process_jmp memory i pointer
          _        -> pointer + 1 + length parameters

        newMemory = case instruction of
          InstrAdd   -> process_math memory i
          InstrMult  -> process_math memory i
          InstrInput -> process_input memory i $ head input
          CmpLT      -> process_compare memory i
          CmpEQ      -> process_compare memory i
          _          -> memory

        newState = case instruction of
          InstrHalt    -> Halted
          InstrInvalid -> Crashed
          _            -> Running

        newInput = case instruction of
          InstrInput -> tail input
          _          -> input

        newOutput = case instruction of
          InstrOutput -> output ++ [process_output memory i]
          _           -> output
    in  Computer { memory = newMemory
                 , state  = newState
                 , ip     = newPointer
                 , input  = newInput
                 , output = newOutput
                 }

data Instruction = Instruction InstructionCode [Parameter]
decodeInstruction :: Memory -> Pointer -> Instruction
decodeInstruction memory pointer =
  let opCode = index memory pointer
      (operation, numberOfParameters) =
          decodeInstructionCode $ opCode `mod` 100
      parameterModes =
          (map decodeParameterMode $ reverse $ toDigits $ opCode `div` 100)
            ++ (Data.List.repeat PositionMode)  -- 0 is default parameter mode
      parameterValues = case numberOfParameters of
        0 -> []
        _ ->
          map (\i -> index memory i)
            $ [(pointer + 1) .. (pointer + numberOfParameters)]
      parameters = zip parameterModes parameterValues
  in  Instruction operation parameters

data InstructionCode =
  InstrAdd
  | InstrMult
  | InstrInput
  | InstrOutput
  | JmpTrue
  | JmpFalse
  | CmpLT
  | CmpEQ
  | InstrHalt
  | InstrInvalid
-- returns instruction code and number of parameters
decodeInstructionCode :: Int -> (InstructionCode, Int)
decodeInstructionCode 1  = (InstrAdd, 3)
decodeInstructionCode 2  = (InstrMult, 3)
decodeInstructionCode 3  = (InstrInput, 1)
decodeInstructionCode 4  = (InstrOutput, 1)
decodeInstructionCode 5  = (JmpTrue, 2)
decodeInstructionCode 6  = (JmpFalse, 2)
decodeInstructionCode 7  = (CmpLT, 3)
decodeInstructionCode 8  = (CmpEQ, 3)
decodeInstructionCode 99 = (InstrHalt, 0)
decodeInstructionCode _  = (InstrInvalid, 0)

data ParameterMode = PositionMode | ImmediateMode | InvalidMode
decodeParameterMode :: Int -> ParameterMode
decodeParameterMode 0 = PositionMode
decodeParameterMode 1 = ImmediateMode
decodeParameterMode _ = InvalidMode

type Parameter = (ParameterMode, Int)
getParameter :: Memory -> Parameter -> Int
getParameter memory (PositionMode , pointer) = index memory pointer
getParameter _      (ImmediateMode, value  ) = value
getParameter _      (InvalidMode  , _      ) = error "invalid parameter mode"

process_math :: Memory -> Instruction -> Memory
process_math memory (Instruction InstrAdd (nounP : verbP : destP : _)) =
  process_math' memory (+) nounP verbP destP
process_math memory (Instruction InstrMult (nounP : verbP : destP : _)) =
  process_math' memory (*) nounP verbP destP

process_math'
  :: Memory
  -> (Int -> Int -> Int)
  -> Parameter
  -> Parameter
  -> Parameter
  -> Memory
process_math' memory ops nounP verbP destP =
  let noun = getParameter memory nounP
      verb = getParameter memory verbP
      dest = case destP of
        (PositionMode, pointer) -> pointer
        _                       -> error "invalid parameter mode for dest"
      result = ops noun verb
  in  update dest result memory

process_input :: Memory -> Instruction -> Int -> Memory
process_input memory (Instruction InstrInput (destP : _)) input =
  let dest = case destP of
        (PositionMode, pointer) -> pointer
        _                       -> error "invalid parameter mode for dest"
  in  update dest input memory

process_output :: Memory -> Instruction -> Int
process_output memory (Instruction InstrOutput (nounP : _)) =
  getParameter memory nounP

process_jmp :: Memory -> Instruction -> Pointer -> Pointer
process_jmp memory (Instruction opP (cmpP : destP : _)) pointer =
  let ops = case opP of
        JmpTrue  -> (/= 0)
        JmpFalse -> (== 0)
        _        -> error "invalid jump instruction"
      cmp        = getParameter memory cmpP
      dest       = getParameter memory destP
      newPointer = case (ops cmp) of
        True  -> dest
        False -> pointer + 3 -- if no jump, ip forwards standard length
  in  newPointer

process_compare :: Memory -> Instruction -> Memory
process_compare memory (Instruction opP (cmpaP : cmpbP : destP : _)) =
  let ops = case opP of
        CmpLT -> (<)
        CmpEQ -> (==)
        _     -> error "invalid compare instruction"
      cmpa = getParameter memory cmpaP
      cmpb = getParameter memory cmpbP
      dest = case destP of
        (PositionMode, pointer) -> pointer
        _                       -> error "invalid parameter mode for dest"
      result = case (ops cmpa cmpb) of
        True  -> 1
        False -> 0
  in  update dest result memory

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
      results = map (\c -> (getMemory c, output c)) $ map (runComputer) $ map
        (\(code, input) -> newComputer (parseProgram code) input)
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
  ]

testExamplesIO :: [(String, [Int], [Int])] -> [(Bool, [Int], [Int])]
testExamplesIO examples =
  let (code, input, expectedOutput) = unzip3 examples
      cases                         = zip code input
      results                       = map output $ map runComputer $ map
        (\(code, input) -> newComputer (parseProgram code) input)
        cases
      checks = map (\(e, r) -> e == r) $ zip expectedOutput results
  in  zip3 checks expectedOutput results

test_code :: Bool
test_code =
  let (checks  , _, _) = Prelude.unzip3 $ testExamplesWithCode examplesWithCode
      (checksIO, _, _) = Prelude.unzip3 $ testExamplesIO examplesIO
  in  all (== True) checks && all (== True) checksIO


-- Helper methods
toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: Integral x => [x] -> x
fromDigits = foldl addDigit 0 where addDigit num d = 10 * num + d
