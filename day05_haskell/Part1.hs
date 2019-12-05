import IntCodeComputer

inputs = [1]

main = 
  if not test_code 
    then error "Interpreter not working"
    else do
      contents <- readFile "input"
      let c = newComputer (parseProgram contents) inputs
      let result = runComputer c
      if state result == Halted
        then do
          putStr "Result: "
          print $ getOutput result
        else do
          putStrLn "Computer crashed"
          print result

