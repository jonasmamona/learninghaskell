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

j :: Monad m => m (m a) -> m a
j a = a >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = a >>= return . f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = a >>= (\x -> b >>= (\y -> return $ f x y))

a :: Monad m => m a -> m (a -> b) -> m b
a x f = x >>= (\x -> f >>= (\g -> return $ g x))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = f x >>= (\x -> meh xs f >>= (\y -> return $ x : y))

flipType :: (Monad m) => [m a] -> m [a]
flipType a = meh a id