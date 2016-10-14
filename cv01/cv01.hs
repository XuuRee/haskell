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


-- power
power :: Double -> Int -> Double
_ `power` 0 = 1
z `power` n = z * (z `power` (n-1))


-- power v.2
power :: Double -> Int -> Double
_ `power` 0 = 1
z `power` n =if n< 0
           then error "error!"
           else z * (z `power` (n-1))


-- fibonacci
fibonacci :: [Int]
fibnacci = error "Not defined!"
