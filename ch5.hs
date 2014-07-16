module Ch5 where

import Ch2


{- 5.1
Implement area with higher order functions
-}

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = let a = distBetween v1 v2
                       b = distBetween v2 v3
                       c = distBetween v3 v1
                       s = 0.5*(a+b+c)
                   in sqrt (s*(s-a)*(s-b)*(s-c))

area :: Shape -> Float
area (Polygon (v1:v2:vs))
    = let fs = map (triArea v1) (v2:vs)        -- list of functions
          res = zipWith ($) fs vs              -- uses function application ($)
      in foldr (+) 0 res
area _               = 0

{- 5.2

map map :: [a -> b] -> [[a] -> [b]]
-------------------------------------------
foldl foldl :: error
-------------------------------------------
map :: (c -> d) -> [c] -> [d]

foldl (a -> b -> a) -> a -> [b] -> a

map foldl :: [a -> b -> a] -> [a -> [b] -> a]

-}

{- 5.3
Rewrite length norecursively
-}

length' :: [a] -> Int
length' xs = foldr one 0 xs
    where one :: a -> Int -> Int
          one _ x = x + 1

{- 5.4
Fill in the two missing functions f1 f2

f1 (f2 (*) [1,2,3,4]) 5

-}

f2 :: (a -> b -> a) -> [a] -> [b -> a]
f2 op xs = map op xs                    -- creates partially applied functions

f1 :: [b -> a] -> b -> [a]
f1 [] _ = []
f1 (f:fs) init = f init : f1 fs init    -- applies the partially applied functions to the argument

{- 5.5
Define the functions:

1. doubleEach [1,2,3] => [2,4,6]

2. pairAndOne [1,2,3] => [(1,2), (2,3), (3,4)]

3. addEachPair [(1,2), (3,4), (5,6)] => [3,7,11]

-}


-- 1. (recursive)
doubleEach :: [Int] -> [Int]
doubleEach []     = []
doubleEach (x:xs) = x * 2 : doubleEach xs

-- 1. (nonrecursive)
doubleEach' :: [Int] -> [Int]
doubleEach' xs = map (*2) xs


-- 2. (recursive)
pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne []     = []
pairAndOne (x:xs) = (x, x + 1) : pairAndOne xs

-- 2. (nonrecursive)
pairAndOne' :: [Int] -> [(Int, Int)]
pairAndOne' xs = zip xs (map (+1) xs)


-- 3. (recursive)
addEachPair :: [(Int, Int)] -> [Int]
addEachPair []            = []
addEachPair ((x1, x2):xs) = x1 + x2 : addEachPair xs

-- 3. (nonrecursive)
addEachPair' :: [(Int, Int)] -> [Int]
addEachPair' xs = map add xs
    where add :: (Int, Int) -> Int
          add (x, y) = x + y

{- 5.6
Define maxList (max of list) and minList (min of list)
-}

maxList,maxList',minList,minList' :: [Int] -> Int

-- recursively
maxList (x:[]) = x
maxList (x:xs) = max x (maxList xs)

-- nonrecursively
maxList' (x:xs) = foldr max x xs

-- recursively
minList (x:[]) = x
minList (x:xs) = min x (minList xs)

-- nonrecursively
minList' (x:xs) = foldr min x xs

{- 5.7
Define function addPairsPointwise

addPairsPointwise [(1,2), (3,4), (5,6)] => (9,12)
-}

-- recursively
addPairsPointwise :: [(Int, Int)] -> (Int, Int)
addPairsPointwise (p:[])      = p
addPairsPointwise ((x, y):ps) = (x + x', y + y')
    where (x', y') = addPairsPointwise ps

-- nonrecursively
addPairsPointwise' :: [(Int, Int)] -> (Int, Int)
addPairsPointwise' (p:ps) = foldr addPair p ps
    where addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
          addPair (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{- 5.8
Frog wants to decrypt and encrypt messages to girlfriend
-}

encrypt,decrypt :: String -> String
encrypt s = map (shift 1) s
decrypt s = map (shift (-1)) s

shift :: Int -> Char -> Char
shift delta c = toEnum (mod ((fromEnum c) + delta + 256) 256)

{- 5.9
MakeChange with given denominations

makeChange 99 [5,1] = [19,4]

Lets make it a greedy version for simplicity
assuming we have sorted the list of denominations
in decreasing order.
-}

-- recursive def
makeChange :: Int -> [Int] -> [Int]
makeChange _ []     = []
makeChange 0 _      = []
makeChange amt (d:ds) = amt `div` d : makeChange (mod amt d) ds


