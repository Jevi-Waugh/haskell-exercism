module Series (slices) where

-- Example -> slices 3 "49142"
-- [[4,9,1],[9,1,4],[1,4,2]]

slices :: Int -> String -> [[Int]]
slices 0 _ = [[]]

slices n (x:xs)
    | n >= length (x:xs)        = [typeCastAll (x:xs)]-- [[read (x:xs) :: Int]]
    |otherwise                  = this
    -- take n (x:xs)

    where
        -- Typecast to integers
        rest = typeCastAll (x:xs)
        this = [take n rest] ++ slices n xs


numTup :: [(Char, Int)]
numTup  = zip ['0'..'9'] [0..9]

typeCast :: Char -> Int
typeCast number = head [i | (s,i) <- numTup, s == number]

typeCastAll :: String -> [Int]
typeCastAll = map typeCast 