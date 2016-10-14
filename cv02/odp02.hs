import Data.Char

-- ascii to string
asciiToString :: [Int] -> String
asciiToString xs = map chr xs


-- odd even split
oddEvenSplit :: [a] -> ([a], [a])
oddEvenSplit [] = ([],[])
oddEvenSplit [a] = ([a],[])
oddEvenSplit (x:y:xs) = (x:xp, y:yp) where (xp, yp) = oddEvenSplit xs

-- f function 
f x = fst x ++ [(&&), (||)]
