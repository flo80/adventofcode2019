module AOC2019.Test.Common where

import           Test.HUnit

testExample :: (Show a, Show b, Eq b) => (a -> b) -> (a, b) -> Test
testExample function (i, o) = TestCase (assertEqual (show i) o (function i))

testFunction :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> Test
testFunction name fct inp res = TestCase (assertEqual name res (fct inp))
