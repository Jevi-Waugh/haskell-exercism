module Yacht (yacht, Category(..)) where

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int

yacht Ones dice =   sum [x | x <- dice, x == 1]
yacht Twos dice =   sum [x | x <- dice, x == 2]
yacht Threes dice = sum [x | x <- dice, x == 3]
yacht Fours dice =  sum [x | x <- dice, x == 4]
yacht Fives dice =  sum [x | x <- dice, x == 5]
yacht Sixes dice =  sum [x | x <- dice, x == 6]

yacht Choice dice = sum dice

yacht Yacht dice = if and [head dice == x | x <- dice] then 50 else 0
-- yacht Yacht dice = if sum [head dice == x | x <- dice] == length dice 50 else 0

yacht FullHouse dice = if n == 2 && (m == 2 || m == 3) then sum dice else 0
    where 
        n = length $ unique dice
        m = length [x | x <- dice, head dice == x]
        
yacht LittleStraight dice
    | (quickSort dice) == [1..length dice]   = 30
    | otherwise                              = 0


yacht BigStraight dice      
    -- we start at 2 not one, which is why we add 1 to len
    | (quickSort dice) == [2.. (+) (pred 2) $ length dice]   = 30
    | otherwise                                              = 0

yacht FourOfAKind dice

    | length (unique dice) == 1  = sum (quickSort dice) - head dice -- doesnt matter what we minus since we have 1 unique number
    | length (unique dice) == 2 && (length xs == 4 || length xs == 1)  = theRest $ quickSort dice
    | otherwise                  = 0 
    where
        theRest (x:xss) = if length xs == 1 then sum xss else sum xs
        xs = [x | x <- dice, x == head dice] 
        

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = (quickSort xss) ++ [x] ++ (quickSort yss)
    where
        -- list of num smaller than x
        xss = [z | z <- xs, z < x]
        -- list of num greater or equal than x
        yss = [z | z <- xs, z >= x]

-- Returns the list of unique numbers
unique :: [Int] -> [Int]
unique [] = []
-- unique (x:xs) = if checkVal x xs then unique xs else (x: unique xs)
unique (x:xs)
    | checkVal x xs   = unique xs -- if elem is there, drop it (ignore it)
    | otherwise       = (x: unique xs) -- if elem is not there, it is unique. keep it

    where 
        checkVal :: Int -> [Int] -> Bool
        checkVal _ [] = False
        -- 1 and [1,2,1,1]s
        -- an if expression is easier here
            -- id is the identify function
        --     any id [False, False, True, False] -→ True

        checkVal num (x:xs) = any id ([num == x] ++ [checkVal num xs])
        -- below is an idea that does not work, head access [] etc..
        -- checkval for each and return list of bool
        -- listBool _ [] = []
        -- listBool x xs = [checkVal x xs] ++ listBool (head xs) xs

        -- -- now we have a list of bool
        -- -- now we filter though
        
        -- uniqNum = zipWith (\x \y -> if y == True then [x] else []) (x:xs) (listBool x xs)
        -- -- 

-- checkVal :: Int -> [Int] -> Bool
-- checkVal _ [] = False
-- checkVal num (x:xs) = any id ([num == x] ++ [checkVal num xs])