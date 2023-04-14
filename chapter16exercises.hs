{-# LANGUAGE FlexibleInstances #-}
module Chapter16Exercises where

import Data.Array

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap :: (a -> b) -> BoolAndSomethingElse a -> BoolAndSomethingElse b
  fmap f (False' a) = False' $ f a
  fmap f (True' a) = True' $ f a

data BoolAndMaybeSomethingElse a = Falsish' | Truish' a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap :: (a -> b) -> BoolAndMaybeSomethingElse a -> BoolAndMaybeSomethingElse b
  fmap _ Falsish' = Falsish'
  fmap f (Truish' a) = Truish' $ f a

newtype Mu f = InF {outF :: f (Mu f)}

data D = D (Array Word Word) Int Int

data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (Second b) = Second b
  fmap f (First a) = First (f a)

data Company a c b = DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company e e') where
  fmap _ (DeepBlue a c) = DeepBlue a c
  fmap f (Something b) = Something (f b)

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap :: forall k (a1 :: k) a2 b. (a2 -> b) -> Flip K a1 a2 -> Flip K a1 b
  fmap f (Flip (K b)) = Flip $ K (f b)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst $ f a

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut $ fmap f a

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious fo fa ft) = Notorious fo fa (fmap f ft)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read g) = Read $ f . g