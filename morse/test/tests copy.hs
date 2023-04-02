import Control.Monad
import Data.Monoid
import Test.QuickCheck
    ( frequency, quickCheck, Arbitrary(arbitrary), CoArbitrary (coarbitrary) )

monoidAssoc :: (Eq m, Monoid m) =>  m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupAssoc :: (Eq m , Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


type S = String
type B = Bool

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada <> b = b
    a <> Nada = a
    Only a <> Only b = Only (a<>b)

instance Semigroup a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
    First' Nada <> b = b
    a <> _ = a

instance Monoid (First' a) where
    mempty = First' Nada
    mappend = (<>)
    
firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return (First' Nada)), (1, return (First' (Only a)))]

type FirstMappend =
    First' String
    -> First' String
    -> First' String
    -> Bool

type FstId =
    First' String -> Bool

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    a <> _ = a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return (Identity a))]


type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq,Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two c d = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>  Semigroup (Three a b c) where 
    Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj _ <> BoolConj False = BoolConj False
    BoolConj False <> BoolConj _ = BoolConj False
    BoolConj True <> BoolConj True = BoolConj True

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj _ <> BoolDisj True = BoolDisj True
    BoolDisj True <> BoolDisj _ = BoolDisj True
    BoolDisj False <> BoolDisj False = BoolDisj False

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst _ <> Snd b = Snd b
    Fst _ <> Fst b = Fst b
    Snd a <> Fst _ = Snd a
    Snd a <> Snd _ = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (\n -> f n <> g n)

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)
    mappend = (<>)
        
type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: OrAssoc)