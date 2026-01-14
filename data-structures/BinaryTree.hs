module BinaryTree where

data Tree a  = Empty | Node (Tree a) a (Tree a) deriving Show

-- inserts an element into a bst
insert :: Ord a => a -> Tree a -> Tree a
insert num (Empty) = Node (Empty) num (Empty)
insert num (Node left value right)
        -- left value - greater values
        | value < num                    = Node left value (insert num right)
        -- right value - lesser values
        | value >= num                    = Node (insert num left) value right
        -- equal value
        -- | x == num                       = 

-- finds an element in a bst
find :: Ord a => a -> Tree a -> Maybe a
find _ Empty = Nothing
find num (Node left value right)
    | value == num = Just num
    | value < num = find num right
    | value >= num = find num left

-- -- mirror image of a tree
mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node left value right) = Node (mirror right) value (mirror left)