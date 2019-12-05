import Data.Foldable (toList)
import Data.Sequence
import System.IO
import Data.List.Split


type Code =  (Seq Int) 
data State = Running | Halted | Crashed
  deriving (Show, Eq)
type Pointer = Int 

data Program = Program {
  code :: Code,
  state :: State,
  ip :: Pointer
} deriving (Show)

new_program :: [Int] -> Program
new_program instructions =
  Program {
    code = fromList instructions,
    state = Running,
    ip = 0
  }

get_program_code :: Program -> [Int]
get_program_code Program{code = c} = toList c

process_program :: Program -> Program
process_program p@Program{code = c, state = s} 
  | s == Halted = p
  | s == Crashed = p
  | otherwise = process_program $ process_opcode p

process_opcode :: Program -> Program
process_opcode p@Program {state = Halted} = p 
process_opcode p@Program {code = c, state = s, ip = pointer} = 
  let instruction = index c pointer
  in case instruction of  
        1 -> process_instruction p (+)
        2 -> process_instruction p (*)
        99 -> Program {code = c, state = Halted, ip = pointer}
        _ -> Program {code = c, state = Crashed, ip = pointer}

process_instruction :: Program -> (Int -> Int -> Int ) -> Program
process_instruction Program {code = c, ip = pointer} operation = 
      let op1 = index c (index c (pointer + 1))
          op2 = index c (index c (pointer + 2))
          dest = index c (pointer + 3) 
          result = operation op1 op2
      in Program {
        code = update dest result c, 
        state = Running, 
        ip = pointer + 4
      }

      
type Example = ([Int],[Int])

examples :: [Example]
examples = [
  ([1,0,0,0,99], [2,0,0,0,99]),
  ([2,3,0,3,99], [2,3,0,6,99]),
  ([2,4,4,5,99,0], [2,4,4,5,99,9801]),
  ([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99]),
  ([1,9,10,3,2,3,11,0,99,30,40,50],[3500,9,10,70,2,3,11,0,99,30,40,50])
  ]

testExamples :: [Example] -> [(Bool, [Int], [Int])]
testExamples examples =
  let 
    (cases, expected) = Prelude.unzip examples
    results = map (get_program_code . process_program . new_program) cases
    checks = map (\(e, r) -> e == r) $ Prelude.zip expected results :: [Bool]
  in 
    Prelude.zip3 checks expected results

test_code :: Bool
test_code = 
  let (checks,_,_) = Prelude.unzip3 $ testExamples examples
  in all (== True) checks

set_special_values :: [Int] -> [Int]
set_special_values (a: _:_:xs) = a : b : c : xs
  where b = 12 :: Int 
        c =  2 :: Int

main = 
    if not test_code 
      then error "Interpreter not working"
      else do
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let input = map read $ splitOn "," contents :: [Int]
        let p = set_special_values input
        let results = (process_program . new_program) p :: Program
        if state results == Halted 
          then do 
            putStrLn "Result: "
            print $ index (code results) 0
          else do 
            putStrLn "Could not get to correct state"
            print results
        hClose handle