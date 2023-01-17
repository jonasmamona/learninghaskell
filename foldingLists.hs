module FoldingLists where 
    myAny f xs = foldr (\x acc -> f x || acc) False xs

    myFoldr :: (a -> b -> b) -> b -> [a] -> b
    myFoldr f z [] = z
    myFoldr f z (x:xs) = f x (myFoldr f z xs)


    -- foldl (flip (*)) 1 [1..3]
    -- (((flip * 1 3) flip * 2) flip * 1)
    -- 3 * 1 * 2 * 1

    mySum :: [Integer] -> Integer
    mySum [] = 0
    mySum (x:xs) = x + mySum xs

    mySumFoldr xs = foldr (+) 0 xs

    fibs20 = 1 : scanl (+) 1 [1..18]
    fibs = 1 : scanl (+) 1 fibs

    fibs100 = 1 : takeWhile (< 100) (scanl (+) 1 fibs)

    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

    factorialL :: (Num a, Enum a) => a -> a
    factorialL n = last (scanl (*) 1 [1..n])