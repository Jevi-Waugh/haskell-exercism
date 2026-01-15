module Prime (nth) where

-- GET ALL NUMBERS THAT ARE DIVISBLE
divisible :: Int -> [Int] -> [Bool]
divisible n integers = map (\x -> (==) ((mod) n x) 0) integers

-- CHECK IF IT IS A PRIME
checkPrimeWorthiness :: Int -> Bool
-- made it more effiecient with square root of n
-- n = a x b
-- so without loss of generality a number a has to always be less than a if not its sqaure root
-- for example n is 15, so a x b is 1 x 15 OR 3 x 5
-- 1 and 3 as seperate computations are both less than sqrt 15
-- So we reduce our search space by O(sqrt(n)) by not repeating known terms 
-- 1 x 15
-- 3 x 5
-- 5 x 3 - repeats
-- 15 x 1 - repeats
-- So we only look for one factorisation and conclude taht it's prime
-- for a rigorous proof look at the end.
-- checkPrimeWorthiness n = length (filter id (divisible n [1..n])) == 2
checkPrimeWorthiness n = length (filter id (divisible n [1..floor $ sqrt (fromIntegral n)])) == 1

-- GET NTH PRIME NUMBER
nth :: Int -> Maybe Integer
nth n
    | n >= 1     = Just $ toInteger $ last $ take n $ filter checkPrimeWorthiness [2..]
    | otherwise  = Nothing


-- FOR IMPLICIT TESTING DO
-- take 13 $ filter checkPrimeWorthiness [2..]

-- PROPER PROOF
-- a x b = n
-- a <= b
-- a x a  <=  b x a 
-- a^2 <= n
-- a <= sqrt(n)
