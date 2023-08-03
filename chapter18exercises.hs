module Chapter18Exercises where

import Control.Monad (join, (>=>))

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

data PhhhbbtttEither b a = MyLeft a | MyRight b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (MyLeft a) = MyLeft (f a)
  fmap _ (MyRight a) = MyRight a

instance Applicative (PhhhbbtttEither b) where
  pure = MyLeft
  (MyLeft f) <*> (MyLeft a) = MyLeft (f a)
  (MyRight a) <*> (MyLeft _) = MyRight a
  _ <*> (MyRight a) = MyRight a

instance Monad (PhhhbbtttEither b) where
  return = pure
  (MyLeft a) >>= f = f a
  (MyRight a) >>= _ = MyRight a

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys    

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons a as) bs = append (a <$> bs) (as <*> bs)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (Cons a b) >>= f = append (f a) (b >>= f)

test = Cons 1 (Cons 2 Nil)

testF = Cons (+1) (Cons (+2) Nil)

wazzup = [1,2]
wazzupf = [(+1), (+2)]