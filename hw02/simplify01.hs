-- simplify01.hs

data Expr a = Con a                     -- Constant value
            | Var String                -- Variable with name
            | Add (Expr a) (Expr a)     -- Addition
            | Mul (Expr a) (Expr a)     -- Multiplication
            deriving ( Show, Read, Eq )

simple :: (Num a) => Expr a -> Expr a
simple (Mul (Con a) (Con b)) = Con (a * b)
simple (Mul (Con a) b)
    | a == 0 = Con 0
    | a == 1 = b
    | otherwise = (Mul (Con a) b)
simple (Mul a (Con b))
    | b == 0 = Con 0
    | b == 1 = a
    | otherwise = (Mul a (Con b))
simple (Add (Con a) (Con b)) = Con (a + b)
simple (Add (Con a) b)
    | a == 0 = b
    | otherwise = (Add (Con a) b)
simple (Add a (Con b))
    | b == 0 = a
    | otherwise = (Add a (Con b))
simple e = e

simplify01 :: Expr Integer -> Expr Integer
simplify01 (Add a b) = simple (Add (simplify01 a) (simplify01 b))
simplify01 (Mul a b) = simple (Mul (simplify01 a) (simplify01 b))
simplify01 e = e
