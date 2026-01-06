module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool

-- isLeapYear year
--   | (((==) 0 $ mod year 4) && not ((==) 0 $ mod year 100)) || ((==) 0 $ mod year 400)  = True
--   | otherwise                                                                          = False

isLeapYear year = (((==) 0 $ mod year 4) && not ((==) 0 $ mod year 100)) || ((==) 0 $ mod year 400)



sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum [x * x | x <- [1..n]]


