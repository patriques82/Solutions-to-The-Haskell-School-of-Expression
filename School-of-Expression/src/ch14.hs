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





