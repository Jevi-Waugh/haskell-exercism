module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite (x:xs) = reciteHeper (x:xs) ++ unwords ["And all for the want of a", x] ++ "."


reciteHeper :: [String] -> String
reciteHeper [] = ""
-- we want a singleton list to terminate as need at least 2 elements to modify a sentence
reciteHeper [_] = ""
reciteHeper (x:y:xs) = unwords ["For want of a", x, "the", y, "was lost."] ++ "\n" ++ reciteHeper (y:xs)