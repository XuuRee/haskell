# hw02

```haskell
data Expr a = Con a                     -- Constant value
            | Var String                -- Variable with name
            | Add (Expr a) (Expr a)     -- Addition
            | Mul (Expr a) (Expr a)     -- Multiplication
            deriving ( Show, Read, Eq )
```
```haskell
eval :: (Num a) => Expr a -> [(String, a)] -> Maybe a
```
