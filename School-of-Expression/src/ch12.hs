module Ch12 where

import Draw hiding (trans)
import SOE

{- 12.1
Prove that the instance of Tree in the class Eq satisfies the
laws of its class.

laws of Eq:
1. (x /= y) = not (x == y)
2. (x == y) && (y == z) = x == z

1.
Leaf x /= Leaf y = not (x == y)
not (x == y) = not (Leaf x == Leaf y)

Branch lx rx /= Branch ly ry = not (Branch lx rx == Branch ly ry)
not (Branch lx rx == Branch ly ry) = not (lx == ly && rx == ry)
not (lx == ly && rx == ry) = not (Branch lx rx == Branch ly ry)

2.
Leaf x == Leaf y && Leaf y == Leaf z = (x == y) && (y == z)
(x == y) && (y == z) = (x == z)
(x == z) = (Leaf x == Leaf z)

Branch lx rx == Branch ly ry && Branch ly ry == Branch lz rz
= (lx == ly && rx == ry) && (ly == lz && ry == rz)
Induction step
(lx == lz && rx == lz) = (Branch lx rx == Branch lz rz)

-}

{- 12.2
Write appropriate instances of Color for the classes: Eq, Ord and Enum.
-}

data Color' = Red'' | Green'' | Blue''

instance Eq Color' where
	Red'   == Red'   = True
	Green' == Green' = True
	Blue'  == Blue'  = True
	_     == _     = False

instance Ord Color' where
	Red' < Green'  = True
	Green' < Blue' = True
	_ < _        = False
	c1 <= c2     = c1 < c2 || c1 == c2

instance Enum Color' where
	toEnum 0       = Red'
	toEnum 1       = Green'
	toEnum 2       = Blue'
	fromEnum Red'   = 0
	fromEnum Green' = 1
	fromEnum Blue'  = 2

{- 12.3
Write a quicksort implementation.
-}

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (< x) xs) ++ [x] ++ quicksort (filter (>= x) xs)

{- 12.4
Define Area, Perimeter, Drawable classes instead of writing area, perimeter and draw
methods for the types Region and Shape.
Skipped
-}

class Area a where
	area :: a -> Float
class Perimeter a where
	perimeter :: a -> Float
class Drawable a where
	draw :: Window -> Color -> a -> IO ()


data Rectangle = Rectangle Float Float -- side
data Ellipse = Ellipse Float Float -- radius


instance Area Rectangle where
	area (Rectangle s1 s2) = s1 * s2
instance Area Ellipse where
	area (Ellipse r1 r2) = pi * r1 * r2

instance Perimeter Rectangle where
	perimeter (Rectangle s1 s2) = s1 * 2 + s2 * 2
instance Perimeter Ellipse where
	perimeter (Ellipse r1 r2) -- based on Ramanujans approximation
		= pi * (3*(r1 + r2) - sqrt ((3*r1 + r2) * (r1 + 3*r2)))

instance Drawable Rectangle where
	draw w c (Rectangle s1 s2)
		= drawInWindow w $
				withColor c $
					drawRegion $
						createRectangle (trans (0,0) (-s1/2, -s2/2)) (trans (0,0) (s1/2, s2/2))
instance Drawable Ellipse where
	draw w c (Ellipse r1 r2)
		= drawInWindow w $
				withColor c $
					drawRegion $
						createEllipse (trans (0,0) (-r1/2, -r2/2)) (trans (0,0) (r1/2, r2/2))

trans :: Vertex -> Vertex -> Point
trans (lx, ly) (x, y)
	= (xWin2 + inchToPixel (lx + x),
		 yWin2 - inchToPixel (ly + y)) -- no scaling for simplification

xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

type Vertex  = (Float,Float)

main :: IO ()
main = runGraphics $
	do
			w <- openWindow "Alternative way" (xWin, yWin)
		 	draw w Blue (Rectangle 6 3)
		 	draw w Red (Ellipse 4 5)
		 	getKey w
		 	closeWindow w

