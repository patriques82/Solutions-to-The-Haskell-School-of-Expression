{- 7.1
Define a higher-order function for fringe and treeSize.
Also define a higher-order function for treeHeight, can
you think of other useful functions defined in terms of
it?
-}

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

-- In-order traverse
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f init (Leaf x)         = f x init
foldTree f init (Branch x t1 t2) = foldTree f (f x (foldTree f init t2)) t1

fringe' :: Tree a -> [a]
fringe' t = foldTree (:) [] t

treeSize' :: Tree a -> Integer
treeSize' t = foldTree (\_ x -> x + 1) 0 t                      -- lambda expression

-- works by ignoring the internal values
exploreStructure :: (a -> a -> a) -> a -> (a -> a) -> Tree b -> a
exploreStructure f leaf branch (Leaf _)
    = leaf
exploreStructure f leaf branch (Branch _ t1 t2)
    = let left = exploreStructure f leaf branch t1
          right = exploreStructure f leaf branch t2
      in branch (f right left)

treeHeight' :: Tree a -> Integer
treeHeight' t = exploreStructure max 0 (+1) t

-- other useful function
leafes :: Tree a -> Integer
leafes t = exploreStructure (+) 1 id t


tree :: Tree Int
tree = Branch 7
        (Branch 3
            (Branch 9
                (Leaf 6)
                (Branch 12
                    (Leaf 22)
                    (Leaf 4)))
            (Branch 15
                (Branch 4
                    (Branch 3
                        (Leaf 8)
                        (Leaf 2))
                    (Leaf 6))
                (Branch 13
                    (Branch 5
                        (Leaf 7)
                        (Leaf 3))
                    (Leaf 7))))
        (Leaf 6)

{- 7.2
Define the following functions

takeTree :: Int -> InternalTree a -> InternalTree a
takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
-}

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
    deriving Show

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree 0 _     = ILeaf
takeTree _ ILeaf = ILeaf
takeTree n (IBranch x t1 t2) = IBranch x (takeTree (n-1) t1) (takeTree (n-1) t2)

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile p ILeaf = ILeaf
takeTreeWhile p (IBranch x t1 t2)
    | otherwise = ILeaf
    | p x       = IBranch x (takeTreeWhile p t1) (takeTreeWhile p t2)

t :: InternalTree Int
t = let t' = IBranch 1 ILeaf ILeaf
    in IBranch 2 t' t'

{- 7.3
Using the InternalTree datatype define tree versions of foldr and repeat

foldr :: (a -> b -> b) -> b -> [a] -> b
repeat :: a -> [a]
-}


-- Pre-order tree traverse
foldrTree :: (a -> b -> b) -> b -> InternalTree a -> b
foldrTree f init ILeaf             = init
foldrTree f init (IBranch x t1 t2) = foldrTree f (foldrTree f (f x init) t1) t2

-- In-order tree traverse
foldrTree' :: (a -> b -> b) -> b -> InternalTree a -> b
foldrTree' f init ILeaf             = init
foldrTree' f init (IBranch x t1 t2) = foldrTree' f (f x (foldrTree' f init t1)) t2

-- Post-order tree traverse
foldrTree'' :: (a -> b -> b) -> b -> InternalTree a -> b
foldrTree'' f init ILeaf             = init
foldrTree'' f init (IBranch x t1 t2) = f x (foldrTree'' f (foldrTree'' f init t1) t2)


repeatTree :: a -> InternalTree a
repeatTree x = IBranch x (repeatTree x) (repeatTree x)

-- Does the same as above but in a different way
repeatTree' :: a -> InternalTree a
repeatTree' x = dup (IBranch x ILeaf ILeaf)
    where dup :: InternalTree a -> InternalTree a
          dup (IBranch y t1 t2) = IBranch y (dup (IBranch y t1 t2)) (dup (IBranch y t1 t2))

{- 7.4
Define tree versions of zip and zipWith for InteralTrees

zip :: [a] -> [b] -> [(a, b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-}

zipTree :: InternalTree a -> InternalTree b -> InternalTree (a, b)
zipTree (IBranch x t1 t2) (IBranch y t3 t4) = IBranch (x, y) (zipTree t1 t3) (zipTree t2 t4)
zipTree _                 _                 = ILeaf

zipTree' :: InternalTree a -> InternalTree b -> InternalTree (a, b)
zipTree' (IBranch x t1 t2) (IBranch y t3 t4)
    = let l = zip (tree2List t1) (tree2List t3)
          r = zip (tree2List t2) (tree2List t4)
      in IBranch (x, y) (list2Tree l) (list2Tree r)

tree2List :: InternalTree a -> [a]
tree2List t = foldrTree'' (:) [] t

list2Tree :: [a] -> InternalTree a
list2Tree [] = ILeaf
list2Tree (x:xs)
    = let half = length xs `div` 2
          lt   = list2Tree (take half xs)
          rt   = list2Tree (drop half xs)
      in IBranch x lt rt

zipWith :: (a -> b -> c) -> InternalTree a -> InternalTree b -> InternalTree c
zipWith f (IBranch x t1 t2) (IBranch y t3 t4)
    = IBranch (f x y) (zipWith f t1 t3) (zipWith f t2 t4)
zipWith f _                 _  = ILeaf










