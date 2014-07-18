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
