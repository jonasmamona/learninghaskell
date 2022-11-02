module MoreFunctionalPatterns where

bindExp :: Integer -> String
bindExp x =
  let x = 10; y = 5
   in "the integer was "
        ++ show x
        ++ " and y was "
        ++ show y

trip :: Integer -> Integer
trip x = (\x -> x * 3) x

question1a x y z = x * y * z

question1b x y = \z -> x * y * z

question1c x = \y -> \z -> x * y * z

question1d = \x -> \y -> \z -> x * y * z

test1 x = \x -> x * x

test2 = \x -> x * x

squareIt x = x * x

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    f = (\n -> n + 1)

addOneIfOdd2 n = case odd n of
  True -> (\n -> n + 1) n
  False -> n

addFive x y = (if x > y then y else x) + 5

addFiveLambda = \x -> \y -> (if x > y then y else x) + 5

mFlipLambda f = \x -> \y -> f y x

mFlip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
