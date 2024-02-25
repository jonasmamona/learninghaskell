module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup key [] = Nothing
myLookup key ((x,y):xs)
    | key == x = Just y
    | otherwise = myLookup key xs

xs :: Maybe Integer
xs = myLookup 3 $ zip x y

ys :: Maybe Integer
ys = myLookup 6 $ zip y z

zs :: Maybe Integer
zs = myLookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = myLookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x,y) = f x y

summed :: Num c => (c,c) -> c
summed (x,y) = myUncurry (+) (x,y)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing = b
mayybe b f (Just a) = f a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = mayybe a id Nothing
fromMaybe' a (Just b) = mayybe a id (Just b)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

appliedTos' :: [Bool]
appliedTos'= sequA $ fromMaybe 0 s'

boltToys :: Bool
boltToys = bolt $ fromMaybe 0 ys

applyAllConditions :: Integral a => a -> Bool
applyAllConditions m = and (sequA m)

main :: IO ()
main = do
    print $
        sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7