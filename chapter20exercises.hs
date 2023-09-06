module Chapter20Exercises where

import Data.Foldable
import Data.Maybe
import Data.Monoid

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b
  foldr f z (Constant b) = f b z

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two x y) = f y
  foldr f z (Two x y) = f y z

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z
  foldr f z (Three a b c) = f c z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z
  foldr f z (Three' a b c) = f b (f c z)

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four a b c d) = f b <> f c <> f d
  foldr f z (Four a b c d) = f b (f c (f d z))

myFilterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
myFilterF f = foldMap (\x -> if f x then pure x else mempty)

test :: [Int]
test = myFilterF even [1..20]