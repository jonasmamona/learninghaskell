module Chapter17 where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid

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
cowFromString name age weight =
  Cow
    <$> noEmpty name
    <*> noNegative age
    <*> noNegative weight

fixerUpper1 = const <$> Just "Hello" <*> pure "World"

fixerUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = fold (append . f) Nil

length' :: List a -> Int
length' Nil = 0
length' (Cons a as) = 1 + length' as

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons a as) bs = append (a <$> bs) (as <*> bs)

list1 = Cons (+ 1) (Cons (* 2) Nil)

list2 = Cons 1 (Cons 2 Nil)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' howMany (Cons a as) =
  if howMany > 0
    then Cons a (take' (howMany - 1) as)
    else Nil

extractList :: ZipList' a -> List a
extractList (ZipList' Nil) = Nil
extractList (ZipList' (Cons a as)) = Cons a as

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (<*>) (ZipList' (Cons a as)) (ZipList' (Cons b bs)) = ZipList' (Cons (a b) (extractList (ZipList' as <*> ZipList' bs)))
  (<*>) (ZipList' Nil) _ = ZipList' Nil 
  (<*>) _ (ZipList' Nil) = ZipList' Nil 

zl' :: List a -> ZipList' a
zl' = ZipList'

z' = zl' $ Cons (+ 9) (Cons (* 2) (Cons (+ 8) Nil))

z2 = zl' $ Cons 1 (Cons 2 (Cons 3 Nil))

test = z' <*> z2

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

data Validation err a = Failure err | Success a deriving (Eq, Show)

instance Semigroup e => Semigroup (Validation e a) where
  (<>) (Failure e) (Failure e') = Failure $ e <> e'
  (<>) (Failure e) _ = Failure e
  (<>) _ (Failure e) = Failure e
  (<>) (Success a) (Success a') = Success a

instance Monoid e => Monoid (Validation e a) where
  mempty = Failure mempty

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success a) = Success $ f a
  (<*>) (Failure f) (Failure e) = Failure $ f <> e
  (<*>) _ (Failure e) = Failure e
  (<*>) (Failure f) _ = Failure f

success :: Validation String Integer
success = Success (+1) <*> Success 1

failure = Success (+1) <*> Failure [StackOverflow]

failure' = Failure [StackOverflow] <*> Success (+1)

failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]

main :: IO ()
main = do
  print test