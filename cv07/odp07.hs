--solution to Exercise 3
countFold :: Eq a => a -> [a] -> Int
countFold x = foldr (\e r -> if x == e then r + 1 else r) 0

--solution to Exercise 4
data Fraction = Fraction Integer Integer
              | NaF
              deriving (Show)

instance Eq Fraction where
    (Fraction a b) == (Fraction c d) = a * d == b * c
    NaF == NaF = True
    _ == _ = False

instance Num Fraction where
    (Fraction a b) + (Fraction c d) = Fraction (a * d + c * b) (b * d)
    _ + _ = NaF

    (Fraction a b) * (Fraction c d) = Fraction (a * c) (b * d)
    _ * _ = NaF

    abs (Fraction a b) = Fraction (abs a) (abs b)
    abs _ = NaF

    signum (Fraction a b) = Fraction (signum (a * b)) 1
    signum _ = NaF

    fromInteger x = Fraction x 1

    negate (Fraction a b) = Fraction (-a) b
    negate _ = NaF


--solution to Exercise 5
insert :: Ord a => a -> [a] -> [a]
insert x s = a ++ (x : b)
    where (a, b) = break (>x) s

insertSort :: Ord a => [a] -> [a]
insertSort = foldr insert []