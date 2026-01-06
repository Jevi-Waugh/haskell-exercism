module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
  | n <= 0            = Nothing
  | number == n       = Just Perfect
  | number < n        = Just Deficient
  | number > n        = Just Abundant

  where
    divisors = [1..n-1]
    perfectDivisors = filter (\d -> (mod n d == 0)) divisors
    number = sum (perfectDivisors)

    -- generate a list from [1..n]
    -- divide everyone by n
    -- save the ones that d % n == 0 into a new list
    -- Dont forget to exlude n
    -- sum that list into `number`

-- shorter solution for all

-- summedNumber = sum $ filter ((== 0) . mod n) [1..n-1]