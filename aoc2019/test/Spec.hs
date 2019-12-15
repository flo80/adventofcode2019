import           Test.HUnit
import           AOC2019.Test.Day1
import           AOC2019.Test.Day3
import           AOC2019.Test.Day12
import           AOC2019.Test.Day14

main :: IO Counts
main = runTestTT $ TestList [day1tests, day3tests, day12tests, day14tests]
