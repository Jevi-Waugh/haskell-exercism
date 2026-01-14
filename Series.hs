module Series (slices) where



slices :: Int -> String -> [[Int]]
slices 0 [] = [[]]

slices n (x:xs)
    | n >= length (x:xs)        = [typeCastAll (x:xs)]-- [[read (x:xs) :: Int]]
    |otherwise                  = this
    -- take n (x:xs)

    where
        rest = typeCastAll (x:xs)
        this = [take n rest] ++ slices n (tail xs)


numTup :: [(Char, Int)]
numTup  = zip ['0'..'9'] [0..9]

typeCast :: Char -> Int
typeCast number = head [i | (s,i) <- numTup, s == number]

typeCastAll :: String -> [Int]
typeCastAll = map typeCast 