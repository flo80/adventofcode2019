import           Test.HUnit
import           AOC2019.Test.IntCodeComputer
import           AOC2019.Test.Day1
import           AOC2019.Test.Day3
import           AOC2019.Test.Day10
import           AOC2019.Test.Day12
import           AOC2019.Test.Day14
import           AOC2019.Test.Day16

main :: IO Counts
main = runTestTT $ TestList
  [ computerTests
  , day1tests
  , day3tests
  , day10tests
  , day12tests
  , day14tests
  , day16tests
  ]
