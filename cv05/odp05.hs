import Data.Char

--solution to Exercise 3
pythagoras :: Int -> [(Int, Int, Int)]
pythagoras n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

--solution to Exercise 4
greet :: IO Bool
greet = putStrLn "Say hello:" >> getLine >>= \y ->
        if map toLower y == "hello"
            then return True
            else putStrLn "You are rude!" >> return False

--solution to Exercise 5
getNumberOfSolutions :: IO Int
getNumberOfSolutions = do
    putStrLn "Enter the equation (ax^2 + bx + c = 0):"
    putStr "a = "
    a <- getLine
    putStr "b = "
    b <- getLine
    putStr "c = "
    c <- getLine
    let d = discriminant (read a) (read b) (read c)
    return (if (d < 0) then 0 else (if d == 0 then 1 else 2))

discriminant :: Int -> Int -> Int -> Int
discriminant a b c = b ^ 2 - 4 * a * c