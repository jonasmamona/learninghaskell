module Chapter7Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDigitDivMod :: Integral a => a -> a
tensDigitDivMod x = d
  where
    xLast = x `div` 10
    d = snd (xLast `divMod` 10)

hunsD :: Integral a => a -> a
hunsD x = d2
  where
    xLast = x `div` 100
    d2 = xLast `mod` 100

hunsDivMod :: Integral a => a -> a
hunsDivMod x = d2
  where
    xLast = x `div` 100
    d2 = snd (xLast `divMod` 100)

myComparison :: a -> a -> Bool -> a
myComparison x y z =
  case z of
    True -> x
    False -> y

myComparisonWithGuards :: a -> a -> Bool -> a
myComparisonWithGuards x y z
  | z = x
  | otherwise = y

myComparisonPattern :: a -> a -> Bool -> a
myComparisonPattern x _ True = x
myComparisonPattern _ y False = y

g :: (a -> b) -> (a,c) -> (b,c)
g f x = (f (fst x), snd x)

