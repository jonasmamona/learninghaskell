module Chapter22 where

import Control.Applicative
import Data.Char
import Control.Monad.Trans.Reader
import Control.Monad

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