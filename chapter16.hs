module Chapter16 where
import Data.List
import Text.Read (readMaybe)

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: [Maybe [Char]] -> [Maybe Char]
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

a = fmap (+1) $ (read "[1]" :: [Int])

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d :: Integer -> [Char]
d n = (return '1' ++) . show $ (\x -> [x, 1..3]) n

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123"++) $ fmap show ioi
    in fmap (*3) changed

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose' f g x = (fmap (g . f) x) == (fmap g . fmap f $ x)

myFunctionComposition :: Integer -> Integer
myFunctionComposition = (+1).(*2)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c deriving (Eq, Show)
 
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Eq)

instance (Show a, Show b) => Show (Four' a b) where
  show (Four' a b c d) = "Four' " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust Nothing = Nothing
incIfJust (Just n) = Just $ n + 1

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust Nothing = Nothing
showIfJust (Just s) = Just $ show s

manyJustInts = [(Just 1), (Just 1), (Just 1), (Just 1),(Just 1),(Just 1), (Just 1)]

manyJustIntsWithNothing = intersperse Nothing manyJustInts

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

manyPossiblyInts = [(Yeppers 1), (Yeppers 1), (Yeppers 1), (Yeppers 1),(Yeppers 1),(Yeppers 1), (Yeppers 1)]

manyPossiblyIntsWithNothing = intersperse LolNope manyPossiblyInts


incIfRight :: Num a => Either e a -> Either e a
incIfRight (Left e) = Left e
incIfRight (Right a) = Right $ a + 1

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Left e) = Left e
showIfRight (Right a) = Right $ show a

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

newtype Constant a b = Constant { getConstant  :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt =  fmap read getLine

type Nat f g = forall a . f a -> g a

