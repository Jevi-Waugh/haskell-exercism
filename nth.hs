module Prime (nth) where


divisible :: Int -> [Int] -> [Bool]
divisible n integers = map (\x -> (==) ((mod) n x) 0) integers

checkPrimeWorthiness :: Int -> Bool
checkPrimeWorthiness n = length (filter id (divisible n [1..n])) == 2

nth :: Int -> Maybe Integer
nth n
    | n >= 1     = Just $ toInteger $ last $ take n $ filter checkPrimeWorthiness [2..]
    | otherwise  = Nothing


