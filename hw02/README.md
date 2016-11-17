# hw02

In this case we work with syntax trees. Data type:

```haskell
data Expr a = Con a                     -- Constant value
            | Var String                -- Variable with name
            | Add (Expr a) (Expr a)     -- Addition
            | Mul (Expr a) (Expr a)     -- Multiplication
            deriving ( Show, Read, Eq )
```
The task is to program three functions for work with algebraic expressions: 

```haskell
eval :: (Num a) => Expr a -> [(String, a)] -> Maybe a
```
```haskell
simplify01 :: Expr Integer -> Expr Integer
```
```haskell
simplifyConstants :: Expr Integer -> Expr Integer,
```


You can see on the examples below how program work.

```haskell
> eval (Add (Con 42) (Mul (Var "a") (Var "b"))) [("a", 2), ("b", 3)]
Just 48
> eval (Add (Con 42) (Mul (Var "a") (Var "b"))) [("a", 2)]
Nothing

> simplify01 (Add (Con 42) (Mul (Var "a") (Var "b")))
Add (Con 42) (Mul (Var "a") (Var "b"))
> simplify01 (Add (Con 0) (Mul (Var "a") (Var "b")))
Mul (Var "a") (Var "b")
> simplify01 (Add (Con 0) (Mul (Con 1) (Var "b")))
Var "b"
> simplify01 (Mul (Var "d") (Mul (Con 4) (Con 0)))
Con 0

> simplifyConstants (Add (Con 0) (Mul (Var "a") (Var "b")))
Add (Con 0) (Mul (Var "a") (Var "b"))
> simplifyConstants (Add (Con 0) (Mul (Con 1) (Con 2)))
Con 2
> simplifyConstants (Add (Mul (Con 42) (Con 1)) (Mul (Var "a") ((Add (Con 4) (Con 1)))))
Add (Con 42) (Mul (Var "a") (Con 5))
```
