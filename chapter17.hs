module Chapter17 where

import Control.Applicative
import Data.List (elemIndex)

embedInt :: Int -> [Int]
embedInt = pure

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup value [] = Nothing
myLookup value list@(x : xs) = if value == fst x then Just $ snd x else myLookup value xs

myTest = [(1, "test"), (2, "test")]

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

q :: Maybe Int
q = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> q

xs = [1, 2, 3]

ys = [4, 5, 6]

w = Just 6

e = Just 5

summed :: Maybe Integer
summed = fmap sum $ (,) <$> w <*> e

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant v) (Constant v') = Constant (mappend v v')

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if length s > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = fmap Address $ validateLength 100 s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Int -> Maybe Person
mkPerson n a b = Person <$> mkName n <*> mkAddress a

justValue :: Maybe a -> a -> a
justValue (Just a) _ = a
justValue Nothing a = a

babe = mkName "babe"

data Cow = Cow {name :: String, age :: Int, weight :: Int} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight = Cow <$> noEmpty name 
                                    <*> noNegative age
                                    <*> noNegative weight

fixerUpper1 = const <$> Just "Hello" <*> pure "World"

fixerUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]