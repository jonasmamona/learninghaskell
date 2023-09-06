module Chapter20 where

import Data.Foldable
import Data.Maybe
import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

mySum :: (Foldable t, Num a) => t a -> a
mySum xs = getSum $ foldMap Sum xs

mySumFoldR :: (Foldable t, Num a) => t a -> a
mySumFoldR xs = foldr (+) 0 xs

myProduct :: (Foldable t, Num a) => t a -> a
myProduct xs = getProduct $ foldMap Product xs

myProductFoldR :: (Foldable t, Num a) => t a -> a
myProductFoldR xs = foldr (*) 1 xs

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x = foldr (\y acc -> acc || (==) x y) False

myElemFoldMap :: (Foldable t, Eq a) => a -> t a -> Bool
myElemFoldMap x xs = getAny $ foldMap (\y -> Any $ x == y) xs

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum = foldr (\x acc -> if acc > Just x || isNothing acc then Just x else acc) Nothing

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum = foldr (\x acc -> if Just x > acc || isNothing acc then Just x else acc) Nothing

myNull :: (Foldable t) => t a -> Bool
myNull xs = length xs == 0

otherNull :: (Foldable t) => t a -> Bool
otherNull = foldr (\_ _ -> False) True

myLength :: (Foldable t) => t a -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myToList :: (Foldable t) => t a -> [a]
myToList xs = foldr (\x acc -> x : acc) [] xs

myFold :: (Foldable t, Monoid m) => t m -> m
myFold xs = foldr (\x acc -> x <> acc) mempty xs

otherFold :: (Foldable t, Monoid m) => t m -> m
otherFold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f xs = foldr (\x acc -> (f x) <> acc) mempty xs