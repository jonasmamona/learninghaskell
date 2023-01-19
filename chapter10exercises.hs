module Chapter10Exercises where

    stops = "pbtdkg"
    vowels = "aeiou"

    nouns = ["Jonas", "cars", "carrots"]

    verbs = ["eats", "lives", "likes"]

    permutateOverVowels firstLetter lastLetter = 
        map (\x -> (firstLetter, x, lastLetter)) verbs

    permutateStops (x:xs) = 
        case xs of
            [] -> []
            _ -> map (\y -> (x, y, head xs)) verbs

    fst' (a,b,c) = a
    
    permutateStopsOverVowels list@(x:xs) acc = 
        case xs of
            [] -> acc
            _ -> permutateStopsOverVowels xs (permutateStops list : acc)

    permutateStopsOverVowelsOnlyP list@(x:xs) acc = 
        case xs of
            [] -> foldr (\current accumulator -> if fst' (head current) == 'p' then current : accumulator else accumulator) [] acc
            _ -> permutateStopsOverVowelsOnlyP xs (permutateStops list : acc)

    seekritFunc x =
        div (sum (map length (words x)))
        (length (words x))

    seekritFuncFractional x =
        (fromIntegral $ (sum (map length (words x)))) / (fromIntegral $ (length (words x)))

    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (x:xs) =
        if x then True
        else myOr xs

    myOr2 :: [Bool] -> Bool
    myOr2 [] = False
    myOr2 (x:xs) = x || myOr2 xs

    myOrFold :: [Bool] -> Bool
    myOrFold = foldr (\x acc -> if x then True else acc) False
    
    myOrFold2 :: [Bool] -> Bool
    myOrFold2 = foldr (||) False

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = False 
    myAny f (x:xs) = if f x then True else myAny f xs

    myAny2 :: (a -> Bool) -> [a] -> Bool
    myAny2 _ [] = False 
    myAny2 f (x:xs) = f x || myAny f xs

    myAnyFoldr :: (a -> Bool) -> [a] -> Bool
    myAnyFoldr f = foldr (\x acc -> f x || acc) False

    myAnyFoldr2 :: (a -> Bool) -> [a] -> Bool
    myAnyFoldr2 f = foldr (\x acc -> f x || acc) False

    myElem :: Eq a => a -> [a] -> Bool
    myElem x = foldr (\a acc -> if a == x then True else acc) False
    
    myElemAny :: Eq a => a -> [a] -> Bool
    myElemAny x = any (\y -> y == x)

    myReverse :: [a] -> [a]
    myReverse xs = go xs []
        where go [] acc = acc 
              go list@(y:ys) acc = go ys (y : acc)

    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr (\x acc -> (f x) : acc) []

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

    squish :: [[a]] -> [a]
    squish = concat

    mySquish :: [[a]] -> [a]
    mySquish = foldr (++) []

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f = foldr (\x acc -> f x ++ acc) []

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy _ [] = error "empty list"
    myMaximumBy f [x] = x
    myMaximumBy f (x:xs) = go (x:xs) x
        where go [] biggest = biggest
              go (y:ys) biggest =
                  case f y biggest of
                      GT -> go ys y
                      _ -> go ys biggest

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy _ [] = error "empty list"
    myMinimumBy f [x] = x
    myMinimumBy f (x:xs) = go (x:xs) x
        where go [] smallest = smallest
              go (y:ys) smallest =
                  case f y smallest of
                      LT -> go ys y
                      _ -> go ys smallest