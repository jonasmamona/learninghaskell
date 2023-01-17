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