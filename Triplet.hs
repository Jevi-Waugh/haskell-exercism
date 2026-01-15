module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = [(x,y,z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], theorem x y z n]


theorem :: Int -> Int -> Int -> Int -> Bool
theorem x y z n = if (x^2) + (y^2) == (z^2) && x + y + z == n then True else False

