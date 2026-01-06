module Hamming (distance) where

distance :: String -> String -> Maybe Int
-- distance xs ys
--   | length xs /= length ys     = Nothing
--   | otherwise                  = Just $ func xs ys

--   where
--     func [] [] = 0
--     func (x:xss) (y:yss) = (if x == y then 0 else 1) + func xss yss


-- Tail recursion
distance xs ys
  | length xs /= length ys     = Nothing
  | otherwise                  = Just $ func xs ys 0

  where
    func [] [] acc = acc
    func (x:xss) (y:yss) acc = func xss yss (acc + if x == y then 0 else 1)

-- OR
-- otherwise = Just $ length $ filter id $ zipWith (/=) xs ys