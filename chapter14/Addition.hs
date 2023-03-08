module Addition where

import Test.Hspec

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

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "2 times 2 is 4" $ do 
      myMultiply 2 2 `shouldBe` 4
    it " 10 times 5 is 50" $ do
      myMultiply 10 5 `shouldBe` 50
    it "12321312 times 21312 is 262591801344" $ do
      myMultiply 12321312 21312 `shouldBe` 262591801344