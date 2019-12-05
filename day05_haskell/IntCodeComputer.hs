module IntCodeComputer 
  (
    Computer
  , State (..)
  , parseProgram
  , newComputer
  , runComputer
  , getMemory
  , getOutput
  , state
  , test_code
  ) where 

import Data.Foldable (toList)
import Data.Sequence (Seq, fromList, index, update)
import Data.List.Split (splitOn)
import Data.List (repeat)


data Computer = Computer {
  memory :: Memory,
  state :: State,
  ip :: Pointer,
  input :: [Int],
  output :: [Int]
} deriving (Show)

type Memory =  (Seq Int) 
data State = Running | Halted | Crashed 
  deriving (Show, Eq)
type Pointer = Int 

parseProgram :: String -> Memory
parseProgram contents =
  fromList $ map read $ splitOn "," contents 

newComputer :: Memory -> [Int] -> Computer
newComputer memory input =
  Computer {
    memory = memory,
    state = Running,
    ip = 0,
    input = input,
    output = []
  }

getMemory :: Computer -> [Int]
getMemory Computer{memory = m} = toList m

getOutput :: Computer -> [Int]
getOutput Computer{output = o} = o

runComputer :: Computer -> Computer
runComputer computer@Computer{state = s} 
  | s == Running  = runComputer $ processInstruction computer 
  | s == Halted   = computer
  | s == Crashed  = computer

processInstruction :: Computer -> Computer
processInstruction computer@Computer {state = Halted} = computer
processInstruction computer@Computer {memory = memory, ip = pointer, input = input, output = output} = 
  let 
    i@(Instruction instruction parameters) = decodeInstruction memory pointer
    newPointer = pointer + 1 + length parameters

    newMemory = case instruction of  
        InstrAdd      -> process_math memory i
        InstrMult     -> process_math memory i
        InstrInput    -> process_input memory i $ head input
        _             -> memory

    newState = case instruction of
        InstrHalt     -> Halted
        InstrInvalid  -> Crashed
        _             -> Running

    newInput = case instruction of
        InstrInput    -> tail input
        _             -> input

    newOutput = case instruction of
        InstrOutput   -> output ++ [process_output memory i]
        _             -> output
  in
    Computer {
      memory = newMemory, 
      state = newState, 
      ip = newPointer, 
      input = newInput, 
      output = newOutput
      }

data Instruction = Instruction InstructionCode [Parameter]
decodeInstruction :: Memory -> Pointer -> Instruction
decodeInstruction memory pointer =
  let
    opCode = index memory pointer
    (operation, numberOfParameters) = decodeInstructionCode $ opCode `mod` 100
    parameterModes  = (map decodeParameterMode $ reverse $ toDigits $ opCode `div` 100) 
                        ++ (Data.List.repeat PositionMode)  -- 0 is default parameter mode
    parameterValues = case numberOfParameters of
                        0 -> []
                        _ -> map (\i -> index memory i ) $ [(pointer+1)..(pointer+numberOfParameters)]
    parameters = zip parameterModes parameterValues
  in 
    Instruction operation parameters

data InstructionCode = InstrAdd | InstrMult | InstrInput | InstrOutput | InstrHalt | InstrInvalid
-- returns instruction code and number of parameters
decodeInstructionCode :: Int -> (InstructionCode, Int)
decodeInstructionCode  1 = (InstrAdd, 3)
decodeInstructionCode  2 = (InstrMult, 3)
decodeInstructionCode  3 = (InstrInput, 1)
decodeInstructionCode  4 = (InstrOutput, 1)
decodeInstructionCode 99 = (InstrHalt, 0)
decodeInstructionCode  _ = (InstrInvalid, 0)

data ParameterMode = PositionMode | ImmediateMode | InvalidMode
decodeParameterMode :: Int -> ParameterMode
decodeParameterMode 0 = PositionMode
decodeParameterMode 1 = ImmediateMode
decodeParameterMode _ = InvalidMode

type Parameter = (ParameterMode, Int)
getParameter :: Memory -> Parameter               -> Int
getParameter    memory    (PositionMode,  pointer) = index memory pointer
getParameter    _         (ImmediateMode, value)   = value
getParameter    _         (InvalidMode,   _)       = error "invalid parameter mode" 

process_math :: Memory -> Instruction -> Memory
process_math memory (Instruction InstrAdd  (nounP : verbP : destP : _)) = process_math' memory (+) nounP verbP destP
process_math memory (Instruction InstrMult (nounP : verbP : destP : _)) = process_math' memory (*) nounP verbP destP

process_math' :: Memory -> (Int -> Int -> Int) -> Parameter -> Parameter -> Parameter -> Memory
process_math'    memory    ops                    nounP        verbP        destP = 
  let 
    noun = getParameter memory nounP
    verb = getParameter memory verbP
    dest = case destP of 
            (PositionMode, pointer) -> pointer
            _                       -> error "invalid parameter mode for dest"
    result = ops noun verb
  in 
    update dest result memory

process_input :: Memory -> Instruction -> Int -> Memory
process_input memory (Instruction InstrInput (destP : _)) input =
  let
    dest = case destP of 
      (PositionMode, pointer) -> pointer
      _                       -> error "invalid parameter mode for dest"
  in
    update dest input memory

process_output :: Memory -> Instruction -> Int 
process_output memory (Instruction InstrOutput (nounP : _)) =
  getParameter memory nounP


-- ((Memory, Input),(Memory, Output))
type Example = ((String,[Int]), ExampleOutput)
type ExampleOutput = ([Int],[Int])

examples :: [Example]
examples = [
  (("1,0,0,0,99",                     []), ([2,0,0,0,99],                         [])),
  (("2,3,0,3,99",                     []), ([2,3,0,6,99],                         [])),
  (("2,4,4,5,99,0",                   []), ([2,4,4,5,99,9801],                    [])),
  (("1,1,1,4,99,5,6,0,99",            []), ([30,1,1,4,2,5,6,0,99],                [])),
  (("1,9,10,3,2,3,11,0,99,30,40,50",  []), ([3500,9,10,70,2,3,11,0,99,30,40,50],  [])),
  (("3,0,4,0,99",                    [0]), ([0,0,4,0,99],                        [0])),
  (("3,0,4,0,99",                   [-1]), ([-1,0,4,0,99],                      [-1])),
  (("1101,100,-1,4,0",                []), ([1101,100,-1,4,99],                   []))
  ]

testExamples :: [Example] -> [(Bool, ExampleOutput, ExampleOutput)]
testExamples examples =
  let 
    (cases, expected) = Prelude.unzip examples
    results = map (\c -> (getMemory c, output c)) $ map (runComputer) $ map (\(code, input) -> newComputer (parseProgram code) input) cases
    checks = map (\(e, r) -> e == r) $ Prelude.zip expected results :: [Bool]
  in 
    Prelude.zip3 checks expected results

test_code :: Bool
test_code = 
  let (checks,_,_) = Prelude.unzip3 $ testExamples examples
  in all (== True) checks


-- Helper methods
toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: Integral x => [x] -> x
fromDigits = foldl addDigit 0
    where addDigit num d = 10*num + d