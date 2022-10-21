module TrainingTypes where

    addStuff :: Integer -> Integer -> Integer
    addStuff a b = a + b + 5

    subtractStuff :: Integer -> Integer -> Integer
    subtractStuff x y = x - y - 10
    
    subtractOne x = subtractStuff x (-9)

    uncurryedSum :: (Integer, Integer) -> Integer
    uncurryedSum (x,y) = x + y
    
    functionC :: (Ord x) => x -> x -> Bool 
    functionC x y = 
        if (x > y) then True else False

    functionS :: () => (a, a) -> a
    functionS (x, y) = y

    myFunc :: (x -> y)
        -> (y -> z)
        -> c
        -> (a, x)
        -> (a, z)
    myFunc xToY yToZ _ (a, x) =
        (a, (yToZ (xToY x)))

    functionA x = x ++ "haskell"
    functionB y = length y

    makingItWork = myFunc functionA functionB "memess" (True, "suck my tiny ")

    i :: a -> a
    i x = x

    c :: a -> b -> a
    c x y = x

    c'' :: b -> a -> b
    c'' x y = x

    c' :: a -> b -> b
    c' x y = y

    r :: [a] -> [a]
    r x = x

    r' :: [a] -> [a]
    r' x = x ++ x

    ca :: b -> c
    ca x = undefined

    cb :: a -> b
    cb x = undefined

    co :: (b -> c) -> (a -> b) -> a -> c
    co ca cb x = (ca (cb x))

    aa :: a -> c
    aa x = undefined

    a :: (a -> c) -> a -> a
    a _ x = x 

    a'a :: a -> b
    a'a x = undefined

    a' :: (a -> b) -> a -> b
    a' a'a x = (a'a x)