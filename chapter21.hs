module Chapter21 where

import Control.Monad
import Data.ByteString.Lazy hiding (map)
import Data.Functor.Sum
import Network.Wreq

urls :: [String]
urls = ["http://httpbin.org/ip", "http://httpbin.org/bytes/5"]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

putStrMappingGet :: IO ()
putStrMappingGet = do
  responses <- sequence mappingGet
  mapM_ (putStrLn . show) responses

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)

newtype Constant a b = Constant {getConstant :: a} deriving (Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Semigroup (Constant a b) where
  _ <> Constant a = Constant a

instance Monoid (Constant a b) where
  mempty = undefined

instance Applicative (Constant a) where
  pure a = undefined
  _ <*> (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

data Optional a = Nada | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  (Yep f) <*> (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse f (Yep a) = fmap Yep (f a)

data List a = Nil | Cons a (List a) deriving (Show)

instance Semigroup (List a) where
  Nil <> a = a
  a <> Nil = a
  (Cons a b) <> c = Cons a (b <> c)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f b) <*> c = (f <$> c) <> (b <*> c)

xs = Cons 1 (Cons 2 (Cons 3 Nil))

ys = Cons (+ 1) (Cons (+ 2) (Cons (+ 3) Nil))

data Three a b c = Three a b c deriving (Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

data Big a b = Big a b b

instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where
  (Big a b c) <> (Big x y z) = Big (a <> x) (y <> b) (y <> b)

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  mempty = Big mempty mempty mempty

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance (Semigroup a, Monoid a) => Applicative (Big a) where
  pure a = Big mempty a a
  (Big a b c) <*> (Big x y z) = Big (a <> x) (b y) (c z)

instance Foldable (Big a) where
  foldMap f (Big _ b c) = f b <> f c

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

data Bigger a b = Bigger a b b b deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Bigger a b) where
  (Bigger a b c d) <> (Bigger x y z w) = Bigger (a <> x) (b <> y) (c <> z) (d <> w)

instance (Monoid a, Monoid b) => Monoid (Bigger a b) where
  mempty = Bigger mempty mempty mempty mempty

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance (Semigroup a, Monoid a) => Applicative (Bigger a) where
  pure a = Bigger mempty a a a
  (Bigger f g h j) <*> (Bigger a b c d) = Bigger (f <> a) (g b) (h c) (j d)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger x y z w) = Bigger x <$> f y <*> f z <*> f w

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node x y z) = Node (f <$> x) (f y) (f <$> z)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node x y z) = foldMap f x <> f y <> foldMap f z

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node x y z) = Node <$> traverse f x <*> f y <*> traverse f z

  