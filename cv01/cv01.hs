-- exercise one


--Same as (+)
myPlus x y = x + y


-- Same as (-)
myMinus x y = x - y


-- logicalAnd v.1
logicalAnd x y = if x
                 then y
                 else False


-- logicalAnd v.2
logicalAnd' True True = True
logicalAnd' False False = False
logicalAnd' _ _ = False


-- roots
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = if d < 0 then error "0" else (x, y)
                 where
                   x = e + sqrt d / (2 * a)
                   y = e - sqrt d / (2 * a)
                   d = b ^ 2 - 4 * a * c
                   e = - b / (2 * a)


-- factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)


-- power
power :: Double -> Int -> Double
_ `power` 0 = 1
z `power` n = if n < 0 then error "negative!" else z * (z `power` (n-1))


-- dfct
dfct :: Integer -> Integer
dfct 0 = 1
dfct 1 = 1
dfct n = n * dfct (n - 2)


-- combinatorial
combinatorial :: Integer -> Integer -> Integer
combinatorial n k = div (factorial n)  ((factorial k) * (factorial(n - k)))


-- digits
digits :: Integer -> Integer
digits 0 = 0
digits n = (mod n 10) + digits (div n 10)

-- mygcd
mygcd :: Integer -> Integer -> Integer
mygcd x y = if y == 0 then x
                      else mygcd (min x y) ((max x y) `mod` (min x y))

