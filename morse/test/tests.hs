import Control.Monad
import Data.Monoid
import Test.QuickCheck




type Verb = String 
type Adjective  = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = e <> "! he said " <> adv <> " as he jumped into his car " <> noun <> " and drove off with his " <> adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said " , adv , " as he jumped into his car " , noun , " and drove off with his " , adj , " wife."]

monoidAssoc :: (Eq m, Monoid m) =>  m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    Twoo <> Fools = Fools
    Twoo <> Twoo = Twoo
    Fools <> Fools = Fools

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend =
    Bull -> Bull -> Bull -> Bool    

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


main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
