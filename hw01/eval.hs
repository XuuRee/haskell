import Data.List

eval :: String -> Int
eval = evalPlusMinus

evalPlusMinus :: String -> Int
evalPlusMinus e = if null r_r then evalMulDivMod e
                              else getFun (head r_r) (evalPlusMinus (reverse (tail r_r)))                  
                                                     (evalPlusMinus (reverse l_r))

    where
        (l_r, r_r) = break (`elem` "+-") (reverse e)
        getFun :: Char -> (Int -> Int -> Int)
        getFun '+' = (+)
        getFun '-' = (-)

evalMulDivMod :: String -> Int
evalMulDivMod e = if null r_r then evalPower e
                              else getFun (head r_r) (evalMulDivMod (reverse (tail r_r)))
                                                     (evalMulDivMod (reverse l_r))

    where
        (l_r, r_r) = break (`elem` "*/%") (reverse e)
        getFun :: Char -> (Int -> Int -> Int)
        getFun '*' = (*)
        getFun '/' = div
        getFun '%' = mod

evalPower :: String -> Int
evalPower e = if null r then evalNum e
                        else getFun (head r) (evalPower l) (evalPower (tail r))

    where
        (l, r) = break (`elem` "^") e
        getFun :: Char -> (Int -> Int -> Int)
        getFun '^' = (^)

evalNum :: String -> Int
evalNum = read