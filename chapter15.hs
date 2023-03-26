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

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Show a, Show b) => Show (Combine a b) where
  show (Combine f) = "Combine " ++ show (f undefined)
    
instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (\n -> f n <> g n)
    
f :: Combine Integer (Sum Integer)
f = Combine $ \n -> Sum (n + 1)

g = Combine $ \n -> Sum (n - 1)

h = unCombine (f <> g) $ 0

newtype Comp a = Comp {unComp :: (a -> a)}

instance Semigroup a => Semigroup (Comp a) where
    Comp f <> Comp g = Comp (f . g)

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Failure a <> Failure b = Failure (a <> b)
    Failure a <> Success b = Success b
    Success a <> Failure b = Success a
    Success a <> Success _ = Success a

failure :: String -> Validation String Int
failure = Failure

success :: Int -> Validation String Int
success = Success

newtype Mem s a = Mem { runMem :: s -> (a,s)}

instance (Show s, Show  a) => Show (Mem s a) where
    show m = "Mem " ++ show (runMem m undefined)

instance Semigroup a => Semigroup (Mem s a) where
    Mem f <> Mem g = Mem $ \s -> let (a, s') = f s
                                     (b, s'') = g s'
                                 in (a <> b, s'')

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)
rmzero = runMem mempty 0
rmleft = runMem (f' <> mempty) 0
rmright = runMem (mempty <> f') 0
main = do
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0