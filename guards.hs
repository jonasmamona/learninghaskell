module Guards where

myAbs :: (Num a, Ord a) => a -> a
myAbs x = if x < 0 then (-x) else x

myAbsGuarded :: Integer -> Integer
myAbsGuarded x
  | x < 0 = -x
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "Right on"
    | otherwise = "not right"

pal :: Eq a => [a] -> Bool
pal xs 
    | xs == reverse xs = True
    | otherwise = False

numbers :: (Num a, Ord a) => a -> a
numbers x 
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1