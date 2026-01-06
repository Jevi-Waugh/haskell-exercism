module ArmstrongNumbers (armstrong) where

armstrong :: (Integral a, Show a) => a -> Bool

armstrong n = toInteger (sum (map (^ numDigits) number)) == toInteger n
    where
        -- remember read expects a string from  - :info
        number = map (\elem -> read [elem] :: Int) (show n)
        numDigits = length number
