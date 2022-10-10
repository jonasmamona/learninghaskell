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