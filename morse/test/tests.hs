import Control.Monad
import Data.Monoid
import Test.QuickCheck
    ( frequency, quickCheck, Arbitrary(arbitrary), CoArbitrary (coarbitrary) )

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose' f g x = (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

f :: (Eq a) => [Identity a] -> Bool
f = functorIdentity

