module Triplet (tripletsWithSum) where

-- Inefficient solution
-- tripletsWithSum :: Int -> [(Int, Int, Int)]
-- tripletsWithSum n = [(x,y,z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], theorem x y z n]

-- theorem :: Int -> Int -> Int -> Int -> Bool
-- theorem x y z n = if (x^2) + (y^2) == (z^2) && x + y + z == n then True else False


-- Paul's solution
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = 
    [(x,y,z) |
    z <- [1..n], 
    y <- [1..n], 
    x <- [n - y - z], 
    x > 0,
    x < y,
    (z^2 - y^2) == (x^2)]

