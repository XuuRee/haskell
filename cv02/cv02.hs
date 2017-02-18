-- exercise two


import Data.Char


-- head
myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "Cant find first element in empty list."


-- tail
myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail [] = error "Cant find elements without first element in empty list. " 


-- getlast element of list
getLast :: [a] -> a
getLast [] = error "Cant find last element in empty list."
getLast [x] = x
getLast (x:xs) = getLast xs 


-- stripLast
stripLast :: [a] -> [a]
stripLast [x] = []
stripLast (x:xs) = x : stripLast xs


-- median of list
median :: [a] -> a
median [] = error "Empty list dont have median"
median [x] = x
median [x, _] = x
median (_:xs) = median (init xs)


-- length
len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + len xs 


-- doubles
doubles :: [a] -> [(a,a)]
doubles (x:y:s) = (x,y) : doubles s
doubles _ = []


-- add one to the element of list
add1 :: [Integer] -> [Integer]
add1 x = map (+1) x


add1 :: [Integer] -> [Integer]
add1 [] = []
add1 (x:xs) = (x + 1) : add1 xs


-- multiply
multiplyN :: Integer -> [Integer] -> [Integer]
multiplyN _ [] = []
multiplyN n (x:xs) = (x * n) : multiplyN n xs


-- my sum
sums :: [[Int]] -> [Int]
sums [] = []
sums (x:xs) = singleSum x : sums xs
    where singleSum [] = 0
          singleSum (x:xs) = x + singleSum xs


-- function apply to list
applyToList :: (a -> b) -> [a] -> [b]
applyToList _ [] = []
applyToList f (x:xs) = f x : applyToList f xs


-- my even
evens :: [Integer] -> [Integer]
evens [] = []
evens (x:xs) = if even x then x : evens xs else evens xs


-- to upper
toUpperStr :: [Char] -> [Char]
toUpperStr x = map toUpper x


-- multiply even element of the list
multiplyEven :: [Integer] -> [Integer]
multiplyEven x = map (*2) (evens x)

multiplyEven' :: [Integer] -> [Integer]
multiplyEven' xs = map (* 2) (filter even xs)


-- more zero
moreZero :: [Double] -> [Double]
moreZero x = filter (>0) x

sqroots :: [Double] -> [Double]
sqroots x = map (sqrt) (moreZero x)

sqroots' :: [Double] -> [Double]
sqroots' = map sqrt . filter (>0)


-- fromend
fromend :: Int -> [a] -> a
fromend x s = (reverse s) !! (x-1)

fromend' :: Int -> [a] -> a
fromend' x s = if x > len then error "Too short"
                          else s !! (len - x)
            where len = length s


-- maxima
maxNumber :: [[Integer]] -> [Integer]
maxNumber [] = []
maxNumber (x:xs) = maximum x : maxNumber xs

maxima :: [[Int]] -> [Int]
maxima s = map maximum s

