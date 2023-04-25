module Chapter18 where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEvenDesugared :: [Integer] -> [Integer]
twiceWhenEvenDesugared xs =
  xs >>= \x ->
    if even x
      then [x * x, x * x]
      else [x * x]

data Cow = Cow {name :: String, age :: Int, weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty [] = Nothing
noEmpty something = Just something

noNegative :: Int -> Maybe Int
noNegative x
  | 0 > x = Nothing
  | otherwise = Just x

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w = do
  namae <- noEmpty n
  agey <- noNegative a
  weighty <- noNegative w
  weightCheck (Cow namae agey weighty)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b 

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First b) _ = First b
  (<*>) _ (First b) = First b
  (<*>) (Second b) (Second c) = Second $ b c

instance Monad (Sum a) where
  return = pure
  (>>=) (Second b) f = f b
  (>>=) (First b) _ = First b

type Founded = Int

type Coders = Int

data SoftwareShop = Shop {founded :: Founded, programmers :: Coders} deriving (Eq, Show)

data FoundedError = NegativeYears Founded | TooManyYears Founded | NegativeCoders Coders | TooManyCoders Coders | TooManyCodersForYears Founded Coders deriving (Eq, Show)

validateFounded :: Int -> Sum FoundedError Founded
validateFounded n
    | n < 0 = First $ NegativeYears n
    | n > 500 = First $ TooManyYears n
    | otherwise = Second n

validateCoders :: Int -> Sum FoundedError Coders
validateCoders n
    | n < 0 = First $ NegativeCoders n
    | n > 500 = First $ TooManyCoders n
    | otherwise = Second n

mkSoftware :: Int -> Int -> Sum FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then 
            First $ TooManyCodersForYears founded programmers
        else 
            Second $ Shop founded programmers