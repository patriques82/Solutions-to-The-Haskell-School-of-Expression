module Ch17 where

{- 17.1
Verify that the instances of Functor for lists and trees are law-abanding.

instance Functor [] where
	fmap f []     = []
	fmap f (x:xs) = f x : fmap f xs

-- 1.
fmap id []
=> []
=> id []

fmap id (x:xs)
=> id x : fmap id xs
=> x : fmap id xs
=> (x:xs)
=> id (x:xs)

-- 2.
fmap (f . g) []
=> []
=> (fmap f . fmap g) []

fmap (f . g) (x:xs)
=> (f (g x)) : fmap (f . g) xs
=> (f (g x)) : fmap (f . g) xs
=> f x' : fmap f xs'
=> fmap f (x':xs')
	where x'  = g x
				xs' = fmap g xs
=> fmap f $ fmap g (x:xs)
=> (fmap f . fmap g) $ (x:xs)


instance Functor Tree where
	fmap f (Leaf x)       = Leaf (f x)
	fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

-- 1.
fmap id (Leaf x)
=> Leaf (id x)
=> Leaf x
=> id (Leaf x)

fmap id (Branch t1 t2)
=> Branch (fmap id t1) (fmap id t2)
=> Branch t1 t2
=> id (Branch t1 t2)

-- 2.
fmap (f . g) (Leaf x)
=> Leaf (f (g x))
=> Leaf (f x')
	where x' = g x
=> fmap f (Leaf x')
=> fmap f $ fmap g (Leaf x)
=> (fmap f . fmap g) $ x

fmap (f . g) (Branch t1 t2)
=> Branch (fmap (f . g) t1) (fmap (f . g) t2)
=> Branch ((fmap f . fmap g) $ t1) ((fmap f . fmap g) $ t2)
=> (fmap f . fmap g) $ (Branch t1 t2)

-}

{- 17.2
Verify the associativity law for (>>), starting with the associativity law for (>>=).



-}


