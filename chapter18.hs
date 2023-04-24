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

data Cow = Cow {name :: String, age :: Int, weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty [] = Nothing
noEmpty something = Just something

noNegative :: Int -> Maybe Int
noNegative x | 0 > x = Nothing
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