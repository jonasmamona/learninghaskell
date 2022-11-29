module Lists where

import Data.Bool

myHead :: [a] -> a
myHead [] = error "cannot find head of empty list"
myHead (x : _) = x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : []) = Nothing
safeTail (_ : x) = Just x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool x y
  | x == y = [x]
  | otherwise = [x, y]

eftBoolCase :: Bool -> Bool -> [Bool]
eftBoolCase True False = []
eftBoolCase x y =
  case compare x y of
    LT -> [x, y]
    GT -> [x, y]
    EQ -> [x]

eftBoolCaseGuard :: Bool -> Bool -> [Bool]
eftBoolCaseGuard x y = case (x, y) of
  (True, False) -> []
  _ | x == y -> [x]
  _ -> [x, y]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = case (x, y) of
  (GT, LT) -> []
  (GT, EQ) -> []
  (EQ, GT) -> [EQ, GT]
  (LT, GT) -> [LT, EQ, GT]
  _ | x == y -> [x]

eftOrdBetter :: Ordering -> Ordering -> [Ordering]
eftOrdBetter x y = case compare x y of
  EQ -> [x]
  GT -> []
  LT -> x : eftOrdBetter (succ x) y

-- >>> eftOrdBetter LT LT
-- >>> eftOrdBetter LT GT
-- >>> eftOrdBetter GT LT
-- [LT]
-- [LT,EQ,GT]
-- []

eftInt :: Int -> Int -> [Int]
eftInt x y = case compare x y of
  EQ -> [x]
  GT -> []
  LT -> go 1 [x] y
  where
    go sum acc count
      | count == 1 = acc
      | count > 0 = go (sum + 1) (acc ++ [x + sum]) (count - 1)

eftIntBetter :: Int -> Int -> [Int]
eftIntBetter x y = case compare x y of
  EQ -> [x]
  GT -> []
  LT -> x : eftIntBetter (x + 1) y

eftIntBetter2 :: Int -> Int -> [Int]
eftIntBetter2 x y = case compare x y of
  EQ -> [x]
  GT -> []
  LT -> x : eftIntBetter2 (succ x) y

eftChar :: Char -> Char -> [Char]
eftChar x y = case compare x y of
  EQ -> [x]
  GT -> []
  LT -> x : eftChar (succ x) y

-- >>> eftIntBetter 1 10
-- [1,2,3,4,5,6,7,8,9,10]

enumFromToAnything :: (Ord a, Enum a) => a -> a -> [a]
enumFromToAnything x y = case compare x y of
  EQ -> [x]
  GT -> []
  LT -> x : enumFromToAnything (succ x) y

myWords :: [Char] -> [[Char]]
myWords x = go x []
  where
    go x acc
      | (takeWhile (/= ' ') x == x) && x == "" = acc
      | length acc == 0 = go (dropWhile (/= ' ') x) ("wallfish" : acc)
      | takeWhile (/= ' ') x == "" = go (dropWhile (== ' ') x) acc
      | takeWhile (/= ' ') x == x = go (dropWhile (/= ' ') x) (acc ++ [takeWhile (/= ' ') x])
      | otherwise = go (dropWhile (/= ' ') x) (acc ++ [takeWhile (/= ' ') x])

celsoSplit :: Char -> String -> [String]
celsoSplit char text =
  let start = takeWhile (/= char) text
      rest = drop 1 . dropWhile (/= char) $ text
   in if char `elem` rest
        then start : celsoSplit char rest
        else [start, rest]

mySplit :: Char -> [Char] -> [[Char]]
mySplit separator input = go input separator []
  where
    go input separator acc
      | (takeWhile (/= separator) input == input) && input == "" = acc
      | takeWhile (/= separator) input == "" = go (dropWhile (== separator) input) separator acc
      | takeWhile (/= separator) input == input = go (dropWhile (/= separator) input) separator (acc ++ [takeWhile (/= separator) input])
      | otherwise = go (dropWhile (/= separator) input) separator (acc ++ [takeWhile (/= separator) input])

myWords2 :: String -> [String]
myWords2 =
  mySplit ' '

myLines :: String -> [String]
myLines =
  mySplit '\n'

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual = ["Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

-- >>> myLines sentences == shouldEqual
-- True

-- >>>"wallfish" ++ dropWhile (/= ' ') "sheryl wants fun"
-- "wallfish wants fun"

mySqr = [1,4,9,16,25,36,49,64,81,100]
mySqrRemainderEven = [x | x <- mySqr, rem x 2 == 0]
mySqrBetween = [(x, y) | x <- mySqr,y <- mySqr,x < 50, y > 50]
mySqrOnlyFive = take 5 [ (x, y) | x <- mySqr,y <- mySqr, x < 50, y > 50 ]

someUpperCase = "This Phrase Has Some Uppercase Letters"

onlyLowerCase = [x | x <- someUpperCase, elem x (['a'..'z'] ++ [' '])]

needsToBecomeAcronym = "National A Space Agency"

acronymGenerator xs = [x | x <- xs, elem x ['A'..'Z']]

-- >>>onlyLowerCase
-- "his hrase as ome ppercase etters"

-- >>> acronymGenerator	needsToBecomeAcronym
-- "NASA"

-- >>> acronymGenerator "Self Contained Underwater Breathing Apparatus"
-- "SCUBA"

mySquare = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

tupleFromBoth = [(x,y) | x <- mySquare, x < 50, y <- myCube, y < 50]

-- >>>tupleFromBoth
-- [(1,1),(1,8),(1,27),(4,1),(4,8),(4,27),(9,1),(9,8),(9,27),(16,1),(16,8),(16,27),(25,1),(25,8),(25,27)]

-- >>>length tupleFromBoth 
-- 15

integerList :: [Int]
integerList = [1..10] :: [Int]

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

mySumFoldr :: [Int] -> Int
mySumFoldr [] = 0
mySumFoldr xs = foldr (+) 0 xs

myMap xs = map (+1) xs 

-- >>>myMap [1,2,3]
-- [2,3,4]

-- >>> map (take 3) [[1..5], [1..5], [1..5]]
-- [[1,2,3],[1,2,3],[1,2,3]]

-- >>> map (\x -> x+1) [1..5]
-- [2,3,4,5,6]

-- >>> map (\x -> x ++ "s") ["jona", "treva", "roupa"]
-- ["jonas","trevas","roupas"]

itIsMystery xs = map (\x -> elem x "aeiou") xs

-- >>>itIsMystery "jonas"
-- [False,True,False,True,False]

foldBool [] _ = []
foldBool xs truthValue = map (\x -> bool x 0 truthValue) xs

filterEven [] = []
filterEven xs = filter (\x -> rem x 2 == 0) xs

filterOdd [] = []
filterOdd xs = filter (\x -> rem x 2 /= 0) xs

filterEvenComprehension [] = []
filterEvenComprehension xs = [x | x <- xs, (\x -> rem x 2 == 0) x]

filterMultiplesOf3 [] = []
filterMultiplesOf3 xs = filter (\x -> rem x 3 == 0) xs

countingMultiplesOfThree = length . filterMultiplesOf3

myFilter = filter (\x -> x `notElem` ["the", "a", "an"]) . words

myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZipWithMyZipWith _ [] = []
myZipWithMyZipWith [] _ = []
myZipWithMyZipWith xs ys = myZipWith (,) xs ys