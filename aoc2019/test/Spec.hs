import           Test.HUnit
import           AOC2019.Test.Day3
import           AOC2019.Test.Day12

main :: IO Counts
main = runTestTT $ TestList [day3tests, day12tests]
