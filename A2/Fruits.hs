module Fruits (Tree (..), smallest) where

{--

*DO NOT* import any modules.
*You are allowed to use anything available in Prelude and any syntax features*



        "LOW-HANGING FRUITS"

You are standing before a line of trees.
You want to gather all the fruits from one of them, but you hate climbing trees.
So, naturally, you are looking for the smallest tree.

Your task is to implement a function that does the job for you.


A tree is represented by the following recursive type:

                "fruit"
                   v
data Tree a = Tree a [Tree a]
                ^        ^
             "trunk" "branches"


Basically, a tree is a trunk with some branches coming out of it.
Each branch is treated as a tree itself.
"fruits" can be of any type. They play no role in this task and can be ignored.
They will be used to enumerate nodes in the examples.

For instance, the following tree

                         "root"
                           v

                           3
                         / | \
                        4  1  7
                       /       \
                      2    ^    8
                     / \   ^
                    6   5  ^    ^
                           ^    ^
                    ^   ^  ^    ^
                        "leaves"


will have this representation:

Tree 3 [
         Tree 4 [ Tree 2 [
                           Tree 6 []
                         , Tree 5 []
                         ]
                ]
       , Tree 1 []
       , Tree 7 [ Tree 8 [] ]
       ]


The height of a Tree is the longest path from its root to some of its leaves.
The tree from the example has height of 4: the longest parts from its root to its leaves are 3-4-2-6 and 3-4-2-5.

Your task is to implement the function

    smallest :: [Tree a] -> Maybe (Tree a)

which solves the problem for you, picking the shortest tree out of the list of trees.
If there are SEVERAL trees with the same height, it should return the one encountered first in the list.
If there are NO trees, it should return Nothing.

Now, here comes the __fun__ part of __fun__ctional programming!
This task would be too boring if all trees had finite heights.
Here’s the catch: trees can have infinite height, and the list of trees can also be infinite.
The function should still work with them.

We guarantee that it will be possible to find the answer for all test inputs using finite time.
For example, we won’t test the function on an infinite list of infinite trees.
An infinite list of trees, __some__ of which have infinite heights, is possible, though.

EXAMPLES

1.
    smallest [] == Nothing

2.
    tree1 = -- The tree from example above
        Tree 3 [
                 Tree 4 [ Tree 2 [
                                   Tree 6 []
                                 , Tree 5 []
                                 ]
                        ]
               , Tree 1 []
               , Tree 7 [ Tree 8 [] ]
               ]
    tree2 = -- Same as above, but the node 8 has another branch.
        Tree 3 [
                 Tree 4 [ Tree 2 [
                                   Tree 6 []
                                 , Tree 5 []
                                 ]
                        ]
               , Tree 1 []
               , Tree 7 [ Tree 8 [ Tree 42 []] ] -- another branch here with leaf 42
               ]

    smallest [tree1, tree2] == Just tree1 -- The two trees have the same height but tree1 comes in the list before tree2
    smallest [tree2, tree1] == Just tree2 -- The two trees have the same height but tree2 comes in the list before tree1

3.
    tree3 = Tree 3400 [tree1, tree2] -- A bigger tree that has the previous two as branches
    smallest [tree3, tree2, tree1] == Just tree2

4.
    tree4 = Tree 0 [] -- The smallest possible tree
    smallest [tree3, tree2, tree4, tree1] == Just tree4

5.
                             -- v Pay attention to self-referencing! This is a tree of infinite height
    tree5 = Tree 101 [tree3, tree5]

    tree6 = Tree 202 [tree3, tree5] -- No self-referencing, but it still contains an infinite tree inside

    tree7 = Tree 228 [tree5, tree6, tree7]

    smallest [tree3, tree2, tree4, tree1, tree5, tree6] == Just tree4
    smallest [tree3, tree2, tree1, tree5, tree6] == Just tree2
    smallest [tree5, tree3, tree7, tree2, tree7, tree1, tree5, tree6] == Just tree2 -- Infinite trees can be everywhere...


This task is worth 10 POINTS.

--}

data Tree a = Tree a [Tree a] deriving (Eq, Show)


smallest :: [Tree a] -> Maybe (Tree a)
smallest [] = Nothing
smallest xs
    | getNum == Nothing = Nothing
    | otherwise         = recoverTreeFromIdx idx xs

    where
        getNum = trickleDepth (map initFrontier xs) 
        Just idx = getNum


-- What if we checked all trees at the same time
-- But we have many branches to do in parrallel?????
-- Back in COMP3702, if we check the frontiers of a tree,
-- so nodes at a specific depth, we can check all at once
-- and see which one is the longest/shortest
-- This solves infinity because it will keep having children

-- so bfs for all trees


-- start a tree's froniter at depth 0
initFrontier  :: Tree a -> [Tree a]
initFrontier tree = [tree]

-- take current frontier and produce next frontier
getAllFrontiers :: [Tree a] -> [Tree a]
getAllFrontiers [] = []
-- find a way to get all branches
getAllFrontiers (Tree _ branches: rest) = branches ++ getAllFrontiers rest

-- if we get frontiers, we have that function to every tree in the original list.
-- and basically get all frontiers settled at specific depth
-- we have a list of trees for every starting tree
findEmptyFrontier :: [[Tree a]] -> Maybe Int
findEmptyFrontier [] = Nothing
findEmptyFrontier (lt:rest) = recurse 0 (lt:rest)
    where
        recurse num (x:xs) = if x == [] then Just num else recurse (num + 1) xs
        recurse _ [] = Nothing

trickleDepth :: [[Tree a]] -> Maybe Int
-- trickle but for eac tree's frontier
trickleDepth (x:xs)
    | findEmptyFrontier (x:xs) == Nothing     = trickleDepth $ (getAllFrontiers x : map getAllFrontiers xs)
    | otherwise                               = findEmptyFrontier (x:xs) -- return

-- recover tree from index
recoverTreeFromIdx :: Int -> [Tree a] -> Maybe (Tree a)
recoverTreeFromIdx _ [] = Nothing
recoverTreeFromIdx num (t:ts) = here 0 (t:ts)
    where
        here _ [] = Nothing
        -- recurse till we find the indexed tree
        here n (t:ts) = if num == n then Just t else here (n + 1) ts











