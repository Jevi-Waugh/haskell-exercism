module Pascal where

pascal ::[[Int]]
-- Simplify this further
pascal = recurse f [1]
  where
    -- previousRow:0 is incorrect
    f previousRow    = zipWith (+)  (0 : previousRow)  (previousRow ++ [0])


recurse :: ([Int] -> [Int]) -> [Int] -> [[Int]]
recurse f b = b: recurse f (f b)
        -- b:fb: f fb
-- Taking the rows of pascal triangle
takeRows :: Int -> [[Int]]
takeRows n = take n pascal


-- The triangle

    --         1
    --       1   1
    --     1   2   1
    --   1   3   3   1
    -- 1   4   6   4   1


    -- we wanna do b, fb, f (fb) and have [b, fb, f (fb)] 
    -- where f is a lambda/function that keeps left and right side and computes
    -- middle values
    -- so has to be building the next pascal row from the previous one
    -- 
    -- so we need 2 lists, left and right
    -- 1 1
    -- 0 1 1
    -- 1 1 0
    -- 1 2 1