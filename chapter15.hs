module Chapter15 where

import Data.Monoid
import Data.Semigroup

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada <> b = b
    a <> Nada = a
    Only a <> Only b = Only (a<>b)

instance Semigroup a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

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

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>  Semigroup (Three a b c) where 
    Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj _ <> BoolConj False = BoolConj False
    BoolConj False <> BoolConj _ = BoolConj False
    BoolConj True <> BoolConj True = BoolConj True

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj _ <> BoolDisj True = BoolDisj True
    BoolDisj True <> BoolDisj _ = BoolDisj True
    BoolDisj False <> BoolDisj False = BoolDisj False

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Fst _ <> Snd b = Snd b
    Fst _ <> Fst b = Fst b
    Snd a <> _ = Snd a

test :: Or Int Int
test = Fst 1 <> Snd 2