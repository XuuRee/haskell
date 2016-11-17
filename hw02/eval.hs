-- eval.hs

data Expr a = Con a                     -- Constant value
            | Var String                -- Variable with name
            | Add (Expr a) (Expr a)     -- Addition
            | Mul (Expr a) (Expr a)     -- Multiplication
            deriving ( Show, Read, Eq )

eval :: (Num a) => Expr a -> [(String, a)] -> Maybe a
eval (Var v) lst = lookup v lst
eval (Con a) _ = Just a
eval (Add a b) lst = do a <- eval a lst
                        b <- eval b lst
                        return(a + b)
eval (Mul a b) lst = do a <- eval a lst
                        b <- eval b lst
                        return(a * b)
