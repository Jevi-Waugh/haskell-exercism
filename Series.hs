module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs
    | n >= length xs        = read xss :: [Int]

    where
        xss = foldr f [] xs
        f = 