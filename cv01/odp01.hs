-- f and g functions
f True True = 3
f False _ = 42
f _ _ = 12

g True = False
g False = True


-- fivaAnd
fiveAnd True True True True True = True
fiveAnd _ _ _ _ _ = False


-- divisorsCount
divisorsLEQCount x = filter ((== 0) . mod x) [1..x]
divisorsCount x = length (divisorsLEQCount x)
