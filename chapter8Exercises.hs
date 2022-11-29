module Chapter8Exercises where

import Data.List (intersperse)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumFromXtoN :: (Eq a, Num a) => a -> a
sumFromXtoN = go 0
  where
    go acc count
      | count == 0 = acc
      | otherwise = go (acc + count) (count - 1)

myMultiply :: (Integral a) => a -> a -> a
myMultiply x y = go x y 0
  where
    go base times acc
      | times == 0 = acc
      | otherwise = go base (times - 1) (acc + base)

data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = go num denom 0
  where
    go n d count
      | denom == 0 = DividedByZero
      | n < d = Result count
      | otherwise = go (n - d) d (count + 1)

mc91 :: (Ord a, Num a) => a -> a
mc91 x
  | 100 >= x = 91
  | otherwise = x - 10

digitToWord :: Int -> String
digitToWord 0 = "Zero"
digitToWord 1 = "One"
digitToWord 2 = "Two"
digitToWord 3 = "Three"
digitToWord 4 = "Four"
digitToWord 5 = "Five"
digitToWord 6 = "Six"
digitToWord 7 = "Seven"
digitToWord 8 = "Eight"
digitToWord 9 = "Nine"

digits :: Int -> [Int]
digits = map (read . (:[])) . show

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

-- >>> fmap (+2) [1,2,3,4,5,6]
-- [3,4,5,6,7,8]
