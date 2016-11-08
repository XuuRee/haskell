--solution to Exercise 2
data Drink
    = Beer Int
    | Wine [String]
    | Water Double
    | Coffee
    deriving (Eq, Show)

--solution to Exercise 3
nminoLast :: NMino -> Int
nminoLast (UnoMino x) = x
nminoLast (DuoMino _ x) = x
nminoLast (TriMino _ _ x) = x

nminoMatch :: NMino -> NMino -> Bool
nminoMatch x (UnoMino a) = nminoLast x == a
nminoMatch x (DuoMino a _) = nminoLast x == a
nminoMatch x (TriMino a _ _) = nminoLast x == a

nmino :: [NMino] -> Bool
nmino [] = True
nmino [x] = True
nmino (a:b:x) = nminoMatch a b && nmino (b:x)

--solution to Exercise 4
filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust (Nothing : xs) = filterJust xs
filterJust ((Just x) : xs) = x : filterJust xs


--solution to Exercise 5
checkMyStudy :: Requirements -> Bool
checkMyStudy (Course _ r2) = r2
checkMyStudy (All rs) = all checkMyStudy rs
checkMyStudy (AtLeast n rs) = length (filter checkMyStudy rs) >= n