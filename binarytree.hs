module BinaryTree where 

    data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

    insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
    insert' b Leaf = Node Leaf b Leaf
    insert' b (Node left a right)
        | b == a = Node left a right
        | b < a = Node (insert' b left) a right
        | b > a = Node left a (insert' b right)

    mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    mapTree _ Leaf = Leaf
    mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

    testTree' :: BinaryTree Integer
    testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
    mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
    
    -- acceptance test for mapTree
    mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

    testTree :: BinaryTree Int
    testTree = Node (Node (Node (Node Leaf 4 Leaf) 10 (Node Leaf 12 Leaf)) 15 (Node (Node Leaf 18 Leaf) 22 (Node Leaf 24 Leaf))) 25 (Node (Node (Node Leaf 18 Leaf) 35 (Node Leaf 44 Leaf)) 50 (Node (Node Leaf 88 Leaf) 70 (Node Leaf 90 Leaf)))

    preorder :: BinaryTree a -> [a]
    preorder Leaf = []
    preorder (Node left a right) = a : (preorder left ++ preorder right)

    preorder' :: BinaryTree a -> [a]
    preorder' Leaf = []
    preorder' (Node left a right) = [a] ++ preorder left ++ preorder right

    inorder :: BinaryTree a -> [a]
    inorder Leaf = []
    inorder (Node left a right) = inorder left ++ [a] ++ inorder right

    postorder :: BinaryTree a -> [a]
    postorder Leaf = []
    postorder (Node left a right) = postorder left ++ postorder right ++ [a]

    sum' :: Int -> Int -> Int
    sum' x acc = x + acc

    foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldTree _ acc Leaf = acc
    foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc right) left)

    foldTree' ::  (a -> b -> b) -> b -> BinaryTree a -> b
    foldTree' _ acc Leaf = acc
    foldTree' f acc (Node left a right) = foldTree' f (f a (foldTree' f acc right)) left