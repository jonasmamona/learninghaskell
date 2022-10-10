module TrainingBasicTypes where
    data Mood = Blah | Woot deriving Show

    changeMood :: Mood -> Mood
    changeMood Blah = Woot
    changeMood _ = Blah

    fst' :: (a,b) -> a
    fst' (a,b) = a
    
    snd' :: (a,b) -> b
    snd' (a,b) = b

    tupFunc :: (Int, [a])
        -> (Int, [a])
        -> (Int, [a])
    tupFunc (a, b) (c, d) = ((a+c), (b++d))

    isPalindrome :: (Eq a) => [a] -> Bool
    isPalindrome x = 
        x == reverse x

    myAbs :: Integer -> Integer
    myAbs x =
        if x > 0 then x
        else (-x)
        
    f :: (a,b) -> (c,d) -> ((b,d), (a,c))
    f (a,b) (c,d) =
        ((b,d), (a,c))

    addOneToLengthOfString :: String -> Int
    addOneToLengthOfString x = 
        (+) (length x) 1

    identityFunction :: a -> a
    identityFunction x = x

    data Pet = Cat | Dog String