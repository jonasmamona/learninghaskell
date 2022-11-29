module Chapter9Exercises where

import Data.Char
import Data.List

filterNotUpperComprehension :: [Char] -> [Char]
filterNotUpperComprehension [] = []
filterNotUpperComprehension xs = [x | x <- xs, isUpper x]

filterNotUpper :: [Char] -> [Char]
filterNotUpper [] = []
filterNotUpper xs = filter isUpper xs

capitalizeFirst :: [Char] -> [Char]
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeAllRecursive :: [Char] -> [Char]
capitalizeAllRecursive [] = []
capitalizeAllRecursive (x:xs) = toUpper x : capitalizeAllRecursive xs

capitalizeAll :: [Char] -> [Char]
capitalizeAll [] = []
capitalizeAll xs = map toUpper xs

getBigHead :: [Char] -> Char
getBigHead = toUpper . head

getBigHeadPattern :: [Char] -> Char
getBigHeadPattern (x:xs) = toUpper x

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x == True
        then True
    else myOr xs

myOrRecursive :: [Bool] -> Bool
myOrRecursive [] = False
myOrRecursive (x:xs) = x || myOrRecursive xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem element = any (== element)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish [[]] = []
mySquish (x:xs) = x ++ mySquish xs

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap _ [] = []
mySquishMap f (x:xs) = f x ++ mySquishMap f xs

-- >>>mySquishMap (\x -> [1,x]) [1,2,3]
-- [1,1,1,2,1,3]

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain [[]] = []
squishAgain (x:xs) = mySquishMap (x++) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) =
    case f x (head xs) of
        LT -> myMaximumBy f (head xs : tail xs)
        EQ -> myMaximumBy f (x : tail xs)
        GT -> myMaximumBy f (x : tail xs)


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) =
    case f x (head xs) of
        LT -> myMinimumBy f (x : tail xs)
        EQ -> myMinimumBy f (x : tail xs)
        GT -> myMinimumBy f (head xs : tail xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare