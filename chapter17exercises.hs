module Chapter17Exercises where

import Control.Applicative (liftA3)

type MyList a = [a]

newtype ListWrapper a = ListWrapper {getList :: MyList a} deriving (Eq, Show)

instance Functor ListWrapper where
  fmap _ (ListWrapper []) = ListWrapper []
  fmap f (ListWrapper (x : xs)) = ListWrapper $ f x : fmap f xs

instance Applicative ListWrapper where
  pure x = ListWrapper [x]
  (<*>) (ListWrapper (f : fs)) (ListWrapper as) = ListWrapper $ (f <$> as) ++ (fs <*> as)

type MyIo a = IO a

newtype IOWrapper a = IOWrapper {getIO :: IO a}

instance Functor IOWrapper where
  fmap f (IOWrapper io) = IOWrapper (fmap f io)

instance Applicative IOWrapper where
  pure x = IOWrapper (pure x)
  (<*>) (IOWrapper f) (IOWrapper a) = IOWrapper (f <*> a)

type MyTuple a b = (a, b)

newtype TupleWrapper a b = TupleWrapper {getTuple :: MyTuple a b} deriving (Eq, Show)

instance Functor (TupleWrapper a) where
  fmap f (TupleWrapper (a, b)) = TupleWrapper (a, f b)

instance Applicative (TupleWrapper a) where
  pure x = TupleWrapper (undefined, x)
  (<*>) (TupleWrapper (a, f)) (TupleWrapper (a', b)) = TupleWrapper (a, f b)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair a a2) (Pair b b2) = Pair (a b) (a2 b2)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Applicative (Two a) where
  pure = Two undefined
  (<*>) (Two a b) (Two c d) = Two a (b d)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Applicative (Three a b) where
  pure = Three undefined undefined
  (<*>) (Three a b a2) (Three c d b2) = Three a b (a2 b2)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f b)

instance Applicative (Three' a) where
  pure x = Three' undefined x x
  (<*>) (Three' a b c) (Three' d e f) = Three' a (b e) (c f)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Applicative (Four a b c) where
  pure = Four undefined undefined undefined
  (<*>) (Four a b c d) (Four e f g h) = Four a b c (d h)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Applicative (Four' a) where
  pure = Four' undefined undefined undefined
  (<*>) (Four' a b c d) (Four' e f g h) = Four' a b c (d h)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)