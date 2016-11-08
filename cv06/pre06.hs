{-
        foldl1 - take list from left side that have at least one element
        foldr1 - take list from right side that have at least one element

        foldl - take list from left side but foldl have one extra parametr.
                Parametr could be anything what will be cooperate with elements
                of lists.
                -> Neni katamorfismus!

        foldr - take list from right side but foldr have one extra parametr.
                Parametr could be anything what will be cooperate with elements
                of lists.
                !!! foldr (-) 3 [3, 4, 2, 1]
                (3 - {4 - [2 - (1 - 3)]}) = 3
                -> Katamorfismus!
-}

myPlusMulti :: Int -> Int -> Int
myPlusMulti a b = div (a * b) (b)

myLogicalAnd :: Bool -> Bool -> Bool
myLogicalAnd a b = a && b

functionFoldlOne :: [Int] -> Int
functionFoldlOne x = foldl1 (myPlusMulti) x

functionFoldl :: Bool -> [Bool] -> Bool
functionFoldl y x = foldl (myLogicalAnd) y x

-- Identity, (:) = function that take element and join to existing list
whatTheHell = foldr (:) [] "What the hell?"

-- Using lambda with foldr
foldrLambda y x = foldr (\x y->(x+1):y) y x

-- Double elements of list
doubleElements y x = foldr (\x y-> x:x:y) y x
