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
