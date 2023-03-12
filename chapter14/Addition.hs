module Addition where

import Test.Hspec
import Test.QuickCheck
import Data.Bool (Bool)

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

myMultiply :: (Eq a, Num a, Ord a) => a -> a -> a
myMultiply a = go a 0 
  where 
    go x acc counter 
      | counter == 0 = acc
      | otherwise = go x (acc + x) (counter - 1)

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do 
  a <- arbitrary
  b <- arbitrary
  return (a,b)


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "2 times 2 is 4" $ do 
      myMultiply 2 2 `shouldBe` 4
    it " 10 times 5 is 50" $ do
      myMultiply 10 5 `shouldBe` 50
    it "12321312 times 21312 is 262591801344" $ do
      myMultiply 12321312 21312 `shouldBe` 262591801344
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)