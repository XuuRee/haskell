-- f function
f [] = "Empty list"
f (x : xs) = x (f xs)


-- hex to Int
hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

hexToInt :: String -> Int
hexToInt hxStr
    | length hxStr /= 0 = (hexChar(last(hxStr)))+(16*hexToInt(init(hxStr)))
    | otherwise         = 0


-- iterateNTimes
iterateNTimes :: Int -> (a -> a) -> (a -> a)
iterateNTimes 0 _ x = x
iterateNTimes n f x = iterateNTimes (n - 1) f (f x)


-- interleave
interleave :: [[a]] -> [a]
interleave [] = []
interleave xs = inter2 (filter (\x -> not (null x)) xs)
   where inter2 xs = map head xs ++ interleave (map tail xs)
