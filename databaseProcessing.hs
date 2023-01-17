module DatabaseProcessingExercise where

    import Data.Time
    
    data DatabaseItem = DbString String
        | DbNumber Integer
        | DbDate UTCTime
        deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase = 
        [
            DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123)
            ),
            DbNumber 9001,
            DbNumber 9001,
            DbNumber 9001,
            DbString "Hello, world!",
                        DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123)
            )
        ]

    isDbDate (DbDate _) = True
    isDbDate _ = False

    isDbNumber(DbNumber _) = True
    isDbNumber _ = False

    filterByDate :: [DatabaseItem] -> [DatabaseItem]
    filterByDate = foldr (\x acc -> if isDbDate x then (:) x acc else acc) []

    filterByNumberAndReturnValues :: [DatabaseItem] -> [Integer]
    filterByNumberAndReturnValues list = foldr (\(DbNumber x) acc -> x : acc) [] [x | x@(DbNumber _) <- list]

    mostRecentDate :: [DatabaseItem] -> DatabaseItem
    mostRecentDate =  maximum . filterByDate

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy _ [x] = x
    myMaximumBy f (x:xs) =
        case f x (head xs) of
            LT -> myMaximumBy f (head xs : tail xs)
            EQ -> myMaximumBy f (x : tail xs)
            GT -> myMaximumBy f (x : tail xs)
    
    myMaximum :: (Ord a) => [a] -> a
    myMaximum = myMaximumBy compare

    myMostRecentDate :: [DatabaseItem] -> DatabaseItem
    myMostRecentDate = myMaximum . filterByDate

    sumOfNumbers :: [DatabaseItem] -> Integer
    sumOfNumbers list = foldr (\(DbNumber x) acc -> x + acc) 0 [DbNumber x | DbNumber x <- list]

    countOfNumbers :: [DatabaseItem] -> Integer
    countOfNumbers  = foldr (\x acc -> acc + (if isDbNumber x then 1 else 0)) 0 
    
    averageOfNumbers :: [DatabaseItem] -> Integer
    averageOfNumbers list = sumOfNumbers list `div` countOfNumbers list