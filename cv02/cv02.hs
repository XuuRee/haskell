-- exercise two


import Data.Char

-- getlast
getLast :: [a] -> a
getLast [] = undefined
getLast [x] = x
getLast (_:xs) = getLast xs

-- median
median :: [a] -> a
median [x] = x
median [x,y] = x
median (_:xs) = median(init(xs))

-- multiplyN
multiplyN :: Integer -> [Integer] -> [Integer]
multiplyN a [] = []
multiplyN a (x:xs) = a * x : multiplyN a xs

-- Evens, filter
evens :: [Integer] -> [Integer]
evens xs = filter even xs

-- toUpperStr, map
toUpperStr :: String -> String
toUpperStr s = map toUpper s

-- vowels
isVowel :: Char -> Bool
isVowel x = elem (toUpper x) "AEIOUY"

vowels :: [String] -> [String]
vowels xs = map (\s -> filter isVowel s) xs
