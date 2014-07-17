module Ch9 where

import Ch2
import Ch8

{- 9.1
Rewrite:

(Polygon pts) `containsS` p
   = let leftOfList = map (isLeftOf p) (zip pts
                         (tail pts ++ [head pts]))
     in and leftOfList

using curry to simplify.
-}

(Polygon pts) `containsS` p
   = let leftOfList = flip map (zip pts (tail pts ++ [head pts]))
         allToLeft = and (leftOfList p)
     in allToLeft

{- 9.2
Show that flip (flip f) is the same as f.

flip :: (a -> b -> c) -> b -> a -> c
f :: x -> y -> z

flip f :: y -> x -> z
flip (flip f) :: x -> y -> z == f

-}

{- 9.3
What is the type of ys in:

xs = [1,2,3] :: [Float]
ys = map (+) xs

map :: (a -> b) -> [a] -> [b]

(+) :: Num c => c -> c -> c
let c = a
    (c -> c) = b
in (+) :: (a -> b)

map (+) :: Num a => [a] -> [a -> a]
map (+) xs :: [Float -> Float]

-}


{- 9.4
Define applyEach :: Num a => [a -> a] -> a -> [a]
-}

apply :: a -> (a -> b) -> b
apply x f = f x

applyEach :: Num a => [a -> a] -> a -> [a]
applyEach fs x = map (apply x) fs

{- 9.5
Define applyAll :: Num a => [a -> a] -> a -> a
-}

applyAll :: Num a => [a -> a] -> a -> a
applyAll fs x = foldr (flip apply) x fs

{- 9.6
Which is the most efficient?

appendl, appendr :: [a] -> [a] -> [a]      -- wrong type signature!
appendl = foldl (flip (++)) []
appendr = foldr (flip (++)) []

appendl xs
=> foldl (flip (++)) [] xs
=> foldl (flip (++) []) xs
=> flip (++) (xs ++ [])
=> ...
=> ys ++ (xs ++ [])
=> zs ++ (ys ++ (xs ++ []))

This function traverses each list item xs, ys once

appendr xs
=> foldr (flip (++)) [] xs
=> foldr (flip (++) []) xs
=> flip (++) ([] ++ xs)
=> ...
=> (xs ++ []) ++ ys
=> ((xs ++ []) ++ ys) ++ zs

This function travese each list more than once, therefore
appendl is the more efficient function.

-}

{- 9.7
Define twice that takes a function and applies it twice to its second argument
-}

twice :: (a -> a) -> a -> a
twice f x = f (f x)

fourTimes = twice twice

sixteenTimes = twice twice twice

sixteenTimes' = twice (twice twice)

{- 9.8
Generalize twice to the function power that takes a function and integer
specifying how many time to apply the function.
-}

power :: (a -> a) -> Int -> a -> a
power f 1 x = f x
power f n x
    | n > 1     = f (power f (n-1) x)
    | otherwise = error "power: non-valid number"

-- Useful functions
multiply x y = power (+x) y 0

-- The types match up, check stab
data Human = Human Integer
           | Dead

stab :: Human -> Human
stab Dead      = Dead
stab (Human 0) = Dead
stab (Human l) = Human (l - 1)

-- Assumes that no human has a life larger than 100
kill :: Human -> Human
kill human = power stab 100 human

{- 9.9
Define function fix f = f (fix f) and declare its type. Define also the function:

    remainder a b = if a < b then a else remainder (a - b) b

with the function fix so that it does not use recursion.
-}

-- To understand this mystic function I recommend to read about it:
-- http://en.wikibooks.org/wiki/Haskell/Fix_and_recursion (especially the section about
-- recursion).
fix :: (a -> a) -> a
fix f = f (fix f)
-- f (f (f (f ... )))

remainder :: Int -> Int -> Int
remainder = fix (\rec y x -> if y < x then y else rec (y - x) x)

{- 9.10
Rewrite this example using function composition of sections:

    map (\x -> (x + 1)/2) xs
-}

f' = map ((/2) . (+1))

{- 9.11
Consider the function:

    map f (map g xs)

Rewrite this function using a function composition and a single call to map.
Then rewrite the function f' with this function.
-}

doubleMap f g = map (f . g)

f'' = doubleMap (/2) (+1)


