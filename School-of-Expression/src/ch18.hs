module Ch18 where

import Region
import Picture
import Data.List (findIndex)

{- 18.1
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

{- 18.2
Verify the associativity law for (>>), starting with the associativity law for (>>=).

m1 >> (m2 >> m3)
=> m1 >>= ((\_ -> m2) >>= (\_ -> m3))   -- definition of (>>)
=> ((m1 >>= (\_ -> m2)) >>= (\_ -> m3)  -- associativity law for (>>=)
=> (m1 >> m2) >>= (\_ -> m3)
=> (m1 >> m1) >> m3

-}

{- 18.3
Rewrite adjust from section 10.4
-}

-- simply moves the hit to front of list if found
adjust' :: [(Color, Region)] -> Coordinate -> Maybe [(Color, Region)]
adjust' regs p = do
	ind <- findIndex (\(_,r) -> r `containsR` p) regs
	let rest = take ind regs ++ drop (ind+1) regs
	return (regs !! ind : rest)

{- new definition of loop

loop' w regs =
		do clearWindow w
			 sequence_ [ drawRegionInWindow w c r | (c,r) <- reverse regs ]
			 (x,y) <- getLBP w
			 case (adjust regs (pixelToInch (x - xWin2),
													pixelToInch (yWin2 - y) )) of
				 Nothing      -> closeWindow w
				 Just newRegs -> loop' w newRegs

-}

{- 18.4
Verify that instances Maybe and List are monad-law-abinding.


instance Monad Maybe where
	Just x >>= k  = k x
	Nothing >>= k = Nothing
	return        = Just
	fail s        = Nothing

-- 1.
return x >>= k
=> Just x >>= k
=> k x

-- 2.
Nothing >>= return
=> Nothing

Just x >>= return
=> return x
=> Just x

-- 3.
Nothing >>= (\x -> k x >>= h)
=> Nothing

Just x >>= (\x' -> k x' >>= h)
=> (\x' -> k x' >>= h) x
=> k x >>= h
=> (Just x >>= k) >>= h


instance Monad [] where
	xs >>= k = concat (map k xs)
	return x = [x]
	fail s   = []

-- 1.
return x >>= k
=> [x] >>= k
=> concat (map k [x])
=> concat [[x']]
=> [x']
=> k x

-- 2.
[] >>= return
=> concat (map return [])
=> concat []
=> []

(x:xs) >>= return
=> concat (map return (x:xs))
=> concat ([x] : map return xs)
=> (x:xs)

-- 3.
[] >>= (\x -> k x >>= h)
=> concat (map (\x -> k x >>= h) [])
=> concat (concat (map (\x -> k x) []) >>= h)
=> concat (concat (map h (concat (map (\x -> k x) []))))
=> concat (concat (map h (concat [])))
=> concat (concat (map h []))
=> concat (map h ([] >>= k))
=> ([] >>= k) >>= h

(x:xs) >>= (\x' -> k x' >>= h)
=> concat (map (\x' -> k x' >>= h) (x:xs))
=> concat (concat (map (\x' -> k x') (x:xs)) >>= h)
=> concat (concat ([x'] : map (\x' -> k x') xs) >>= h)
=> concat ((x':xs') >>= h)
=> ((x:xs) >>= k) >>= h

-}

{- 18.5
create a instance of Monad for Id
-}

data Id a = Id a

instance Monad Id where
	Id a >>= k = k a
	return     = Id

{- laws

1.
return a >>= k
=> Id a >>= k
=> k a

2.
Id a >>= return
=> return a
=> Id a

3.
Id a >>= (\x -> k x >>= h)
=> (\x -> k x >>= h) $ a
=> k a >>= h
=> (Id a >>= k) >>= h

-}

{- 18.6
Verify that the instances of MonadPlus for the Maybe and list data type are
law-abinding.

instance MonadPlus Maybe where
	mzero              = Nothing
	Nothing `mplus` ys = ys
	xs `mplus` ys      = xs

1.
Nothing >>= (\x' -> mzero)
=> Nothing
=> mzero

Just x >>= (\x' -> mzero)
=> (\x' -> mzero) $ x
=> mzero

2.
mzero >>= m
=> Nothing >>= m
=> Nothing
=> mzero

3.
Nothing `mplus` mzero
=> mzero
=> Nothing

Just x `mplus` mzero
=> Just x

4.
mzero `mplus` Nothing
=> Nothing `mplus` Nothing
=> Nothing

mzero `mplus` Just x
=> Nothing `mplus` Just x
=> Just x


instance MonadPlus [] where
	mzero = []
	mplus = (++)

1.
[] >>= (\x' -> mzero)
=> []
=> mzero

(x:xs) >>= (\x' -> mzero)
=> concat (map (\x' -> mzero) (x:xs))
=> concat ([] : map (\x' -> mzero) xs)
=> []
=> mzero

2.
mzero >>= m
=> [] >>= m
=> concat (map m [])
=> []
=> mzero

3.
[] `mplus` mzero
=> [] ++ mzero
=> [] ++ []
=> []

xs `mplus` mzero
=> xs ++ mzero
=> xs ++ []
=> xs

4.
mzero `mplus` []
=> [] ++ []
=> []

mzero `mplus` xs
=> [] ++ xs
=> xs

-}


data Tree a = Leaf a | Branch (Tree a) (Tree a)
	deriving Show

newtype Label a = Label (Int -> (Int, a))

instance Monad Label where
	return x
		= Label (\s -> (s, x))
	Label ts1 >>= fsm1
		= Label (\s0 ->
				let
					(s1, v1)  = ts1 s0
					Label ts2 = fsm1 v1
				in ts2 s1)

mlabel :: Tree a -> Tree Int
mlabel t = let Label lb = mlab t
					 in snd (lb 0)

mlab :: Tree a -> Label (Tree Int)
mlab (Leaf x)
	= Label (\n -> (n+1, Leaf n))
mlab (Branch t1 t2)
	= do lb1 <- mlab t1
	     lb2 <- mlab t2
	     return (Branch lb1 lb2)

mtest = let t = Branch (Leaf 'a') (Leaf 'b')
        in mlabel (Branch t t)



