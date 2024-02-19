
{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative
import Data.Char
import Control.Monad.Reader

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

myWay :: [Char] -> ([Char], [Char])
myWay x = (cap x, rev x)

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledMonad :: [Char] -> ([Char], [Char])
tupledMonad = do
  a <- cap
  b <- rev
  return (a, b)

tupledMonadWithBind:: [Char] -> ([Char], [Char])
tupledMonadWithBind = cap >>= (\x -> rev >>= (\y -> return (x, y)))

myAsk :: ReaderT (m a) m a
myAsk = ReaderT id

integerReader :: Monad m => ReaderT (m Integer) m Integer
integerReader = myAsk

stringReader :: Monad m => ReaderT (m String) m String
stringReader = myAsk

runTest :: IO ()
runTest = do
  result <- runReaderT stringReader (return "hello")
  putStrLn result
  result <- runReaderT integerReader (return 1)
  print result

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName , dogName :: DogName , address :: Address }deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName, dogsAddress :: Address} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 a b c = a <$> b <*> c

asks :: (r -> m a) -> ReaderT r m a
asks = ReaderT

newtype MyReader r a = MyReader {runMyReader :: r -> a}

instance Functor (MyReader r) where
  fmap f (MyReader a) = MyReader $ \r -> f (a r)

instance Applicative (MyReader r) where
  pure :: a -> MyReader r a
  pure r = MyReader $ const r

  (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
  (MyReader rab) <*> (MyReader ra) = MyReader $ \r -> rab r (ra r)