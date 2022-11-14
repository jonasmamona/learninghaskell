module Factorial where

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+ 1)

three = inc . inc . inc $ 0

three' = (inc . inc . inc) 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes x n = incTimes (x - 1) (inc n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

-- applyTimes 5 (+1) 5
-- (+1) (applyTimes (5-1) (+1) 5)
-- (+1) (+1) (applyTimes (4-1) (+1) 5)
-- (+1) (+1) (+1) (applyTimes (3-1) (+1) 5)
-- (+1) (+1) (+1) (+1) (applyTimes (2-1) (+1) 5)
-- (+1) (+1) (+1) (+1) (+1) (applyTimes (1-1) (+1) 5)
-- (+1) . (+1) . (+1) . (+1) . (+1) $ (5)