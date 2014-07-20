{- 11.1
Prove that:
	putCharList cs = map putChar cs

	putCharList :: [Char] -> [IO ()]
	putCharList []     = []
	putCharList (c:cs) = putChar c : putCharList cs

Base case:
putCharList []
=> []
=> map putChar []

Induction step:
putCharList (c:cs)
=> putChar c : putCharList cs
=> putChar c : map putChar cs
=> map putChar (c:cs)


Prove that:

	listProd xs = foldr (*) 1 xs

	listProd :: [Float] -> Float
	listProd []     = 1
	listProd (x:xs) = x * listProd xs

Base case:
listProd []
=> 1
=> foldr (*) 1 []

Induction step:
listProd (x:xs)
=> x * listProd xs
=> x * foldr (*) 1 xs
=> foldr (*) 1 (x:xs)

-}


{- 11.2 Skipped -}


{- 11.3

Which of the following functions are strict:

reverse, simple, map, tail, area, regionToGRegion, (&&), (True &&), (False &&),

ifFun :: Bool -> a -> a -> a
ifFun pred cons alt = if pred then cons else alt

reverse [] = [] which is bottom
reverse (x:xs) = reverse xs ++ [x] of which the term [x] is bottom and therefore by induction
alse the term reverse xs. The function is strict.

simple x y z = x * (y + z)
since all operators are strict in the function the function is also strict on all its variables.

map f (x:xs) = f x : map f xs
this function is strict only if the function f is strict.

skipped rest...

-}

{- 11.4
Make (^!) more efficient
-}

--(^!)            :: Integer -> Integer -> Integer
x^!n | n<0       = error "negative exponent"
     | otherwise = ff x n
     where ff x n | n==0      = 1
                  | even n    = ff (x*x) (n `quot` 2)
                  | otherwise = ff (x*x) ((n-1) `quot` 2)  -- change this line


{- 11.5
Prove that fac1 n = fac2 n for all nonnegative numbers.

We will do it reverse to simplify

Base case:
fac2 0
=> fac' 0 1
=> 1
=> fac1 0

Induction step (use auxiliary property):
fac2 n
=> fac' n 1
=> fac' (n-1) (n*x)
=> n * fac' (n-1) x 				: property 1
=> n * fac1 (n-1)
=> fac1 n

property 1:
fac' (n-1) (n*x) = n * fac' (n-1) x

Base case:
fac' 0 (1*x)
=> (1*x)
=> 1 * fac' 0 x

Induction step:
fac' n ((n+1)*x)
=> fac' (n-1) (n*(n+1)*x)
=> (n+1) * fac' (n-1) (n*x)

-}

fac1 :: Integer -> Integer
fac1 0 = 1
fac1 n = n * fac1 (n-1)

fac2 :: Integer -> Integer
fac2 n = fac' n 1
	where fac' 0 x = x
				fac' n x = fac' (n-1) (n*x)


