module ReverseString (reverseString) where

-- reverseString :: String -> String
-- reverseString [] = []
-- -- inefficient
-- reverseString (x:xs) = reverseString xs ++ [x]

-- using tail recursion
reverseString :: String -> String
reverseString s = rever s []
  where
    rever [] acc = acc 
    rever (x:xs) acc = rever xs (x:acc)