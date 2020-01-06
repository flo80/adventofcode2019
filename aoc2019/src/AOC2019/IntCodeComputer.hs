{-# LANGUAGE RecordWildCards #-}

module AOC2019.IntCodeComputer
  ( Computer(..)
  , Memory
  , Value
  , State(..)
  , newComputer
  , interactiveComputer
  , runComputer
  , stepComputer
  , resetOutput
  , addInput
  , Disassembler
  , newDisassembler
  )
where

import           Data.Foldable                            ( toList )
import           Data.Sequence                            ( Seq
                                                          , fromList
                                                          , lookup
                                                          , update
                                                          , (><)
                                                          , replicate
                                                          , index
                                                          , drop
                                                          , take
                                                          )
import           Data.List.Split                          ( splitOn )
import           Data.List                                ( repeat
                                                          , nub
                                                          )
import           Data.Maybe                               ( fromMaybe )
import           System.Environment
import           System.Exit

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

instance Show Instruction where
  show (InstrAdd noun verb dest) =
    "add " ++ show noun ++ show verb ++ show dest ++ " "
  show (InstrMult noun verb dest) =
    "mul " ++ show noun ++ show verb ++ show dest ++ " "
  show (InstrInput  dest ) = "in " ++ show dest ++ "\t "
  show (InstrOutput val  ) = "out " ++ show val ++ "\t "
  show (JmpTrue  cmp dest) = "jnz " ++ show cmp ++ show dest ++ "\t "
  show (JmpFalse cmp dest) = "jz " ++ show cmp ++ show dest ++ "\t "
  show (CmpLT cmpA cmpB dest) =
    "lt  " ++ show cmpA ++ show cmpB ++ show dest ++ " "
  show (CmpEQ cmpA cmpB dest) =
    "eq  " ++ show cmpA ++ show cmpB ++ show dest ++ " "
  show (InstrSetBase val) = case val of
    ImmediateMode v -> case v <= 0 of
      True  -> "mbp " ++ show val ++ "\t\t"
      False -> "mbp +" ++ show val ++ "\t\t"

    _ -> "mbp  " ++ show val ++ "\t\t"
  show (InstrHalt        ) = "hlt  \t\t"
  show (InstrInvalid code) = "Instr_" ++ show code ++ " "

data Parameter
  = PositionMode Pointer
  | ImmediateMode Value
  | RelativeMode Pointer
  | InvalidMode Int

instance Show Parameter where
  show (PositionMode  pointer) = "[" ++ (show pointer) ++ "] "
  show (ImmediateMode value  ) = show value ++ " "
  show (RelativeMode pointer)
    | pointer >= 0 = "[bp+" ++ (show pointer) ++ "] "
    | pointer < 0  = "[bp-" ++ (show $ abs pointer) ++ "] "
  show (InvalidMode m) = "?" ++ show m


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
  | s == Running         = runComputer $ stepComputer computer
  | s == WaitingForInput = computer
  | s == Halted          = computer
  | s == Crashed         = computer

stepComputer :: Process
stepComputer computer = processInstruction computer $ fetchInstruction computer

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



-- Disassembler

data Disassembler = Disassembler {
  -- item, jmpsFrom, writesFrom
  lineItems :: [(LineItem,[Pointer], [Pointer])],
  jmpPoints :: [Reference],
  writePoints :: [Reference]
}
type Reference = (String, Pointer) -- label, dest
instance Show Disassembler where
  show d =
    let showLine (li, jumps, writes) =
            show li
              ++ (case length jumps of
                   0 -> ""
                   _ -> "\t; Jumps from: " ++ show jumps
                 )
              ++ (case length writes of
                   0 -> ""
                   _ -> "\t; Writes from: " ++ show writes
                 )
    in  unlines $ map showLine $ lineItems d


newDisassembler :: String -> Disassembler
newDisassembler contents =
  let
    instructions = disassemble' (parseProgram contents) 0 [] :: [LineItem]

    jmpList      = foldl (\acc i -> acc ++ gatherJump i) [] instructions -- dest,src
     where
      gatherJump li@LineItem { content = Instruction (JmpTrue _ (ImmediateMode dest)) }
        = [(dest, fst $ position li)]
      gatherJump li@LineItem { content = Instruction (JmpFalse _ (ImmediateMode dest)) }
        = [(dest, fst $ position li)]
      gatherJump _ = []

    jmpPoints =
      nub $ map (\(d, s) -> ("jmp_" ++ show d, d)) jmpList :: [Reference]

    writeList = foldl (\acc i -> acc ++ gatherWrite i) [] instructions -- dest, src
     where
      gatherWrite li@LineItem { content = Instruction (InstrAdd _ _ (PositionMode dest)) }
        = [(dest, fst $ position li)]
      gatherWrite li@LineItem { content = Instruction (InstrMult _ _ (PositionMode dest)) }
        = [(dest, fst $ position li)]
      gatherWrite li@LineItem { content = Instruction (InstrInput (PositionMode dest)) }
        = [(dest, fst $ position li)]
      gatherWrite li@LineItem { content = Instruction (CmpLT _ _ (PositionMode dest)) }
        = [(dest, fst $ position li)]
      gatherWrite li@LineItem { content = Instruction (CmpEQ _ _ (PositionMode dest)) }
        = [(dest, fst $ position li)]
      gatherWrite _ = []

    dbList = foldl (\acc i -> acc ++ gatherDB i) [] instructions :: [Reference] -- label, dest
     where
      gatherDB li@LineItem { label = Just l, content = Data _ } =
        [(l, fst $ position li)]
      gatherDB _ = []

    writePoints =
      dbList
        ++ (nub $ map (\(d, s) -> ("wri_" ++ show d, d)) writeListWithoutDB) :: [ Reference
        ]
     where
      writeListWithoutDB =
        filter (\(d, s) -> not $ d `elem` destList) writeList
      destList = snd $ unzip dbList

    labledItems = map (addLabel jmpPoints)
      $ map (addLabel writePoints) instructions
     where
      addLabel list instruction = instruction { label = newLabel }
       where
        (a, b)   = position instruction
        range    = [a .. b]
        pot      = nub $ map fst $ filter (\(_, d) -> d `elem` range) list
        newLabel = case length pot of
          1 -> Just (head pot)
          _ -> label instruction

    lineItems = map addReferences labledItems
     where
      addReferences instruction = (instruction, jmpsFrom, writesFrom)
       where
        (a, b)     = position instruction
        range      = [a .. b]
        jmpsFrom   = map snd $ filter (\(d, _) -> d `elem` range) jmpList
        writesFrom = map snd $ filter (\(d, _) -> d `elem` range) writeList
  in
    Disassembler { lineItems   = lineItems
                 , jmpPoints   = jmpPoints
                 , writePoints = writePoints
                 }


data LineItem = LineItem {
  label :: Maybe String,
  content :: Item,
  assembly :: [Value],
  position :: (Pointer,Pointer)
}
instance Show LineItem where
  show li =
    let l = case label li of
          Nothing -> "\t\t"
          Just l  -> l ++ ":\t"
        (a, b) = position li
    in  l
          ++ (show $ content li)
          ++ "\t\t;; "
          ++ show a
          ++ ": "
          ++ (show $ assembly li)

data Item = Instruction Instruction | Data Value
instance Show Item where
  show (Instruction i) = show i
  show (Data        v) = "DB " ++ show v ++ "\t\t"

disassemble :: String -> String
disassemble contents =
  let code         = parseProgram contents
      instructions = disassemble' code 0 []
  in  unlines $ map show instructions

disassemble' :: Memory -> Pointer -> [LineItem] -> [LineItem]
disassemble' memory ip acc
  | ip >= length memory
  = acc
  | otherwise
  = let (item, newIP) = dissInstr memory ip
    in  disassemble' memory newIP (acc ++ [item])

dissInstr :: Memory -> Pointer -> (LineItem, Pointer)
dissInstr memory ip =
  let
    (item, newIP) = case decodeInstruction memory ip of
      Just (instr, size) -> (Instruction instr, ip + size)
      Nothing            -> (Data $ index memory ip, ip + 1)
    l = case ip of
      0 -> Just "_start"
      _ -> case item of
        Data _ -> Just ("val_" ++ show ip)
        _      -> Nothing
    ass =
      toList $ Data.Sequence.take (newIP - ip) $ Data.Sequence.drop ip memory
    li = LineItem { label    = l
                  , content  = item
                  , assembly = ass
                  , position = (ip, newIP - 1)
                  }
  in
    (li, newIP)

decodeInstruction :: Memory -> Pointer -> Maybe (Instruction, Int)
decodeInstruction memory ip' =
  let opCode = index memory ip'

      (instrCode, parameterModes) =
          let instrCode = opCode `mod` 100
              parameterModes =
                  (reverse $ toDigits $ opCode `div` 100) ++ (Data.List.repeat 0)  -- 0 is default parameter mode
          in  (instrCode, parameterModes) :: (Value, [Int])

      mp nr =
          let mode = parameterModes !! (nr - 1)
              val  = index memory (ip' + nr)
          in  case mode of
                0 -> PositionMode val
                1 -> ImmediateMode val
                2 -> RelativeMode val
                _ -> InvalidMode mode
  in  case instrCode of
        1  -> Just (InstrAdd (mp 1) (mp 2) (mp 3), 4)
        2  -> Just (InstrMult (mp 1) (mp 2) (mp 3), 4)
        3  -> Just (InstrInput (mp 1), 2)
        4  -> Just (InstrOutput (mp 1), 2)
        5  -> Just (JmpTrue (mp 1) (mp 2), 3)
        6  -> Just (JmpFalse (mp 1) (mp 2), 3)
        7  -> Just (CmpLT (mp 1) (mp 2) (mp 3), 4)
        8  -> Just (CmpEQ (mp 1) (mp 2) (mp 3), 4)
        9  -> Just (InstrSetBase (mp 1), 2)
        99 -> Just (InstrHalt, 1)
        _  -> Nothing

-- main 

main = do
  args <- System.Environment.getArgs
  case length args of
    0 -> usage >> System.Exit.exitFailure
    1 -> usage >> System.Exit.exitFailure
    _ -> do
      let (cmd, filename, input) = parseArgs args
      contents <- readFile filename
      case cmd of
        "disas" -> putStrLn $ show $ newDisassembler contents
        "run"   -> putStrLn $ show $ interactiveComputer contents input
        _       -> usage

usage = putStrLn "run with 'run filename input' or 'disas filename'"

parseArgs :: [String] -> (String, String, [Int])
parseArgs args = case length args of
  2 -> (args !! 0, args !! 1, [])
  _ -> (args !! 0, args !! 1, map read $ Prelude.drop 2 args)
