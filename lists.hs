module Lists where

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
