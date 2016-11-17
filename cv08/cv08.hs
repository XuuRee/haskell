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
treeFlip :: BinTree a -> BinTree a
treeFlip = treeFold (\v l r -> Node v r l) Empty

-- j) treeId
treeId :: BinTree a -> BinTree a
treeId = treeFold (\v l r -> Node v l r) Empty
treeId’ = treeFold Node Empty

-- k) rightMostBranch
rightMostBranch :: BinTree a -> [a]
rightMostBranch = treeFold (\v l r -> v:r) []

-- l) 
treeRoot :: BinTree a -> a
treeRoot = treeFold (\v l r -> v) undefined
treeRoot’ = treeFold (const . const) undefined

-- m) 
treeNull :: BinTree a -> Bool
treeNull = treeFold (\v l r -> False) True

-- n) 
leavesCount :: BinTree a -> Int
leavesCount = treeFold (\v l r -> if l + r == 0 then 1 else l + r) 0

-- o) 
leavesList :: BinTree a -> [a]
leavesList = treeFold (\v l r -> if null l && null r then [v]
                                                     else l ++ r) []

-- p) 
treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f = treeFold (\v l r -> Node (f v) l r) Empty
treeMap’ f = treeFold (\v -> Node (f v)) Empty
treeMap’’ f = treeFold (Node . f) Empty

-- q) 
treeAny :: (a -> Bool) -> BinTree a -> Bool
treeAny p = treeFold (\v l r -> p v || l || r) False
treeAny’ p = treeFold (\v l r -> or [p v, l, r]) False

-- r) 
treePair :: Eq a => BinTree (a,a) -> Bool
treePair = treeFold (\(x,y) l r -> x == y && l && r) True

-- s) 
subtreeSums :: Num a => BinTree a -> BinTree a
subtreeSums = treeFold (\v l r -> Node (v + root l + root r) l r) Empty
                where root (Node v l r) = v
                      root Empty = 0

-- t) 
findPredicates :: a -> BinTree (Int, a -> Bool) -> [Int]
findPredicates x = treeFold (\(n,v) l r -> if v x then l ++ [n] ++ r
                                                  else l ++ r) []
