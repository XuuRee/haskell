-- simplifyConstants.hs

data Expr a = Con a                     -- Constant value
            | Var String                -- Variable with name
            | Add (Expr a) (Expr a)     -- Addition
            | Mul (Expr a) (Expr a)     -- Multiplication
            deriving ( Show, Read, Eq )

simple :: (Num a) => Expr a -> Expr a
simple (Mul (Con a) (Con b)) = Con (a * b)
simple (Add (Con a) (Con b)) = Con (a + b)
simple e = e

simplifyConstants :: Expr Integer -> Expr Integer
simplifyConstants (Add a b) = simple (Add (simplifyConstants a) (simplifyConstants b)                                       )
simplifyConstants (Mul a b) = simple (Mul (simplifyConstants a) (simplifyConstants b)                                       )
simplifyConstants e = e
