module Ch14 where

import Data.List

fibs :: [Int]
fibs = 1:1:zipWith (+) fibs (tail fibs)

type Response = Integer
type Request = Integer

client :: [Response] -> [Request]
client ys = 1:ys

server :: [Request] -> [Response]
server xs = map (+1) xs

reqs = client resps
resps = server reqs

{- 14.1
Given a list of primes ps, write a recursive stream representing the the
list sequence of numbers that are divisible by no primes other than 1 or
an element of ps.
-}

primes :: [Integer]
primes = primes' [2..]

primes' :: [Integer] -> [Integer]
primes' (p:xs) = p : filter (not . divisibleBy p) xs
	where divisibleBy p x = rem x p == 0

-- wires the numerators to the mappings function so that it can map the
-- head of the numerators to the primes
numerators :: [Integer] -> [Integer]
numerators ns = mappings ns

-- maps the head of the numerators to the primes and passes the mapped
-- values back with a appended recursive call to numerators function with
-- the mapped values so that they also get mapped later.
mappings :: [Integer] -> [Integer]
mappings (n:ns) = ms ++ numerators ms
	where ms = map (n*) ps

-- outputs the mappings of the primes in a stream
stream :: [Integer]
stream = 1 : numerators stream

-- set amount of primes to use
ps = take 5 primes

{- 14.2
Prove as many of the properties as you can of the following proporties, but
for infinite lists (streams).

map properties:
1. map (\x -> x) = \x -> x

Base case:
map (\x -> x) bottom
=> bottom
=> (\x -> x) bottom

Inductive case:
map (\x -> x) (x:xs)
=> (\x -> x) $ x : map (\x -> x) xs
=> x : map (\x -> x) xs
=> inductive step
=> x : (\x -> x) xs
=> x : xs
=> (\x -> x) (x:xs)


2. map (f . g) = map f . map g

Base case:
map (f . g) bottom
=> bottom
=> map f . map g $ bottom

Inductive case:
map (f . g) (x:xs)
=> f . g $ x : map (f . g) xs
=> f (g x) : map (f . g) xs
=> inductive step
=> f (g x) : map f . map g $ xs
=> map f . map g $ (x:xs)

-- skipped these
map f . tail = tail . map f
map f . reverse = concat . map (map f)
map f (xs ++ ys) = map f xs ++ map f ys
f . head = head . map f
(++) properties:
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
xs ++ [] = [] ++ xs = xs

-}

{- 14.3
Prove that fib n = fibs !! n for all n >= 0

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

Base case:
fib 0
=> 1
=> fibs !! 0

fib 1
=> 1
=> fibs !! 1

Inductive case:
fib (n+1)
=> fib n + fib (n-1)
=> inductive step
=> fibs !! n + fibs !! (n-1)
=> fibs !! (n+1)

-}

{- 14.4
Describe each of the following infinite lists.

xs = 1 : map (*2) xs

takes the output of itself (xs) and maps (*2) to each of its elements:
=> 1 : map (*2) (1:xs)
=> 1 : 2 : map (*2) (2:xs)
=> 1 : 2 : 4 : map (*2) (4:xs)
=> 1 : 2 : 4 : 8 : map (*2) (8:xs)
...
this leads to a list of the powers of 2


ys = [1] : [zipWith (+) (0:q) (q++[0]) | q <- ys]

takes the output if itself (ys) and zips together the output with itself
where the first list is appended with a zero at the beginning and the latter
with a zero at the end.
=> [1] : zipWith (+) [0,1] [1,0] : [zipWith (+) (0:q) (q++[0]) | q <- ys]
=> [1] : [1,1] : [zipWith (+) (0:q) (q++[0]) | q <- ys]
=> [1] : [1,1] : zipWith (+) [0,1,1] [1,1,0] : [zipWith (+) (0:q) (q++[0]) | q <- ys]
=> [1] : [1,1] : [1,2,1] : [zipWith (+) (0:q) (q++[0]) | q <- ys]
=> [1] : [1,1] : [1,2,1] : zipWith (+) [0,1,2,1] [1,2,1,0] : [zipWith (+) (0:q) (q++[0]) | q <- ys]
=> [1] : [1,1] : [1,2,1] : [1,3,3,1] : [zipWith (+) (0:q) (q++[0]) | q <- ys]
...
this leads to pascals triangle!

-}







