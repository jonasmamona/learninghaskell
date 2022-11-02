module CaseExpressionsExercises where

functionC x y =
  case bigger of
    True -> x
    False -> y
  where
    bigger = x > y

ifEvenAdd2 n =
  case even of
    True -> n + 2
    False -> n
  where
    even = not (odd n)

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

myFlip f x y = f y x