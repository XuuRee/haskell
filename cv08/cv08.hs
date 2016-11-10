data BinTree a = Node a (BinTree a) (BinTree a)
               | Empty
               deriving (Eq, Show)

treeFold :: (a -> b -> b -> b) -> b -> BinTree a -> b
treeFold n e (Node v l r) = n v (treeFold n e l)
                                (treeFold n e r)
treeFold n e Empty        = e

tree01 :: BinTree Int
tree01 = Node 2 (Node 3 (Node 5 Empty Empty) Empty)
                (Node 4 (Node 1 Empty Empty) (Node 1 Empty Empty))

tree02 :: BinTree String
tree02 = Node "C" (Node "A" Empty (Node "B" Empty Empty))
                  (Node "E" (Node "D" Empty Empty) Empty)

tree03 :: BinTree (Int,Int)
tree03 = Node (3,3) (Node (2,1) Empty Empty)
                    (Node (1,1) Empty Empty)

tree04 :: BinTree a
tree04 = Empty

tree05 :: BinTree Bool
tree05 = Node False (Node False Empty (Node True Empty Empty))
                    (Node False Empty Empty)

tree06 :: BinTree (Int, Int -> Bool)
tree06 = Node (0,even) (Node (1,odd) (Node (2,(== 1)) Empty Empty) Empty)
                       (Node (3,(< 5)) Empty (Node (4,((== 0) . mod 12))
                        Empty Empty))

-- a) treeSum
treeSum :: Num a => BinTree a -> a
treeSum = treeFold (\v l r -> v + l + r) 0

-- b) treeProduct
treeProduct :: Num a => BinTree a -> a
treeProduct = treeFold (\v l r -> v * l * r) 1

-- c) treeOr
treeOr :: BinTree Bool -> Bool
treeOr = treeFold (\v l r -> v || l || r) False

-- d) treeSize
treeSize :: BinTree a -> Int
treeSize = treeFold (\_ l r -> 1 + l + r) 0

-- e) treeHeight
treeHeight :: BinTree a -> Int
treeHeight = treeFold (\_ l r -> 1 + max l r) 0

-- f) treeList
treeList :: BinTree a -> [a]
treeList = treeFold (\v l r -> l ++ [v] ++ r) []

-- g) treeConcat
treeConcat :: BinTree [a] -> [a]
treeConcat = treeFold (\v l r -> l ++ v ++ r) []

-- h) treeMax
treeMax :: (Ord a, Bounded a) => BinTree a -> a
treeMax = treeFold (\v l r -> maximum [v,l,r]) minBound

-- i) treeFlip

-- j) treeId

-- k) rightMostBranch
rightMostBranch :: BinTree a -> [a]
rightMostBranch = treeFold (\v l r -> v:r) []
