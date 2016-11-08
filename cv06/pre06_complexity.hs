-- complexity

-- Complexity: n + 1 + 1 + 2 + 3 + ... + n
reverseListA :: [a] -> [a]
reverseListA [] = []
reverseListA (x:s) = reverseListA s ++ [x]

-- Complexity: 1 + n + 1
reverseListB :: [a] -> [a]
reverseListB = rev []
               where rev s [] = s
                     rev s (x:t) = rev (x:s) t

-- Algorithm B is better

fib :: Integer -> Integer
fib = f 0 1
    where f a _ 0 = a
          f a b k = f b (a+b) (k-1)

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = fib2 (n-2) + fib2 (n-1)

-- fib2 complexity is exponential, fib is linear -> better!