module Fibonacci where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

showFib :: Int -> String
showFib = show . fibonacci

showManyFib :: Int -> [String]
showManyFib x = map showFib [0..x]

showManyFibGo :: Int -> [String]
showManyFibGo x = go x 0
    where go x y
            | y > x = []
            | otherwise = showFib y : go x (y+1)