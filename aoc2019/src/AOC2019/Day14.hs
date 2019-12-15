module AOC2019.Day14
  ( day14a
  , day14b
  , day14run
  )
where

import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                               ( fromJust
                                                          , fromMaybe
                                                          )
import           Text.ParserCombinators.ReadP
import           Control.Applicative                      ( (<|>) )
import           Control.Monad                            ( void )
import           Data.Char                                ( isDigit
                                                          , isAlpha
                                                          )
import           Debug.Trace                              ( traceShowId )

day14run :: IO ()
day14run = do
  contents <- readFile "input/day14"
  putStr "Day 14 - Part 1: "
  print $ day14a contents
  putStr "Day 14 - Part 2: "
  print $ day14b contents
  putStrLn ""

type Amount = Int
type Product = String
type Ingredient = (Product, Amount)
data Production = Production
  { target :: Ingredient
  , sources :: [Ingredient]
  } deriving (Show,Eq)


-- example input: 2 AB, 3 BC, 4 CA => 1 FUEL  
parseInput :: String -> Map Product Production
parseInput contents = Map.fromList $ map (\x -> (fst $target $x, x)) products
 where
  products :: [Production]
  products = fst $ head $ readP_to_S allRules contents

allRules :: ReadP [Production]
allRules = do
  products <- sepBy1 rule ruleEnd
  skipMany (char '\n')
  eof
  return products

 where
  ruleEnd :: ReadP ()
  ruleEnd = void (string "\n") <|> eof

  rule :: ReadP Production
  rule = do
    sources <- sepBy1 ingredient (string ", ")
    _       <- string " => "
    target  <- ingredient
    return (Production target sources)

  ingredient :: ReadP Ingredient
  ingredient = do
    amount  <- many1 (satisfy isDigit)
    _       <- satisfy (== ' ')
    product <- many1 (satisfy isAlpha)
    return (product, read amount)


day14a :: String -> Amount
day14a contents = produce ("FUEL", 1) $ parseInput contents

produce :: Ingredient -> Map Product Production -> Int
produce (tp, ta) rules = res
 where
  (res, _, _) = produce' (0, Map.singleton tp ta, Map.empty)
  produce'
    :: (Int, Map Product Amount, Map Product Amount)
    -> (Int, Map Product Amount, Map Product Amount)
  produce' (ore, wanted, available)
    | length wantedList == 0 = (ore, wanted, available)
    | otherwise              = reduce (ore, wanted, available) tgt
   where
    wantedList = filter (\(p, v) -> v /= 0) $ Map.toList wanted
    tgt        = head wantedList


    reduce
      :: (Int, Map Product Amount, Map Product Amount)
      -> Ingredient
      -> (Int, Map Product Amount, Map Product Amount)
    reduce (oldOre, oldWanted, oldAvailable) ("ORE", tAmount) = produce'
      ( oldOre + (fromMaybe 0 $ Map.lookup "ORE" oldWanted)
      , Map.delete "ORE" oldWanted
      , oldAvailable
      )

    reduce (oldOre, oldWanted, oldAvailable) (tProduct, tAmount) =
      case Map.lookup tProduct oldAvailable of
        Just 0 ->
          produce' (oldOre, oldWanted, Map.delete tProduct oldAvailable)
        Just availAmount -> produce'
          ( oldOre
          , Map.insert tProduct (tAmount - consumed) oldWanted
          , Map.insert tProduct (availAmount - consumed) oldAvailable
          )
          where consumed = minimum [tAmount, availAmount]
        Nothing -> produce'
          $ substitute (oldOre, oldWanted, oldAvailable) (tProduct, tAmount)

    substitute (oldOre, oldWanted, oldAvailable) (tProduct, tAmount) =
      (oldOre, newWanted, newAvailable)
     where
      rule       = fromJust $ Map.lookup tProduct rules
      stepSize   = snd $ target rule
      multiplier = divUp tAmount stepSize
      addWanted =
        Map.fromList $ map (\(p, a) -> (p, a * multiplier)) $ sources rule
      newWanted = Map.unionWith (+) addWanted $ Map.delete tProduct oldWanted
      newAvailable =
        Map.insert tProduct (multiplier * stepSize - tAmount) oldAvailable



divUp :: Int -> Int -> Int
divUp a b = case (x, y) of
  (x, 0) -> x
  (x, _) -> (x + 1)
  where (x, y) = divMod a b

day14b :: String -> Int
day14b contents = binSearch 1 1000000000000 1000000000000
 where
  available :: Int
  available = 1000000000000

  rules = parseInput contents

  binSearch :: Int -> Int -> Int ->  Int
  binSearch min max _ | max < min = max
  binSearch min max target        = case compare res target of
    LT -> binSearch (mid + 1) max target
    GT -> binSearch min (mid - 1) target
    EQ ->  mid
   where
    mid = (min + max) `div` 2
    res = produce ("FUEL", mid) rules
