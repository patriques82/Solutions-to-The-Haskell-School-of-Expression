module Ch13 where

import Region
import Picture
import Animation

{- 13.1
Create instances of Regions and Shapes in the class Combine.
Can you think of reasonable instances of String and Float?
-}

instance Combine Region where
	empty = Empty
	over  = Union

instance Combine Shape where
	empty = undefined -- Can´t come up with nice way of doing this. ??
	over  = undefined

-- Since Combine [a] is already defined in Animation there is no need
-- to redo it. a is a polymorphic type variable and could be a Char
-- String = [Char] => Combine String is not necessary

instance Combine Float where
	empty = 0
	over  = (+)

{- 13.2
Make the planets more realistic:
-}

-- Result of implementing spec 1,2:

-- earths perimeter around sun
perimeter :: (Behavior Time, Behavior Time)
perimeter = (2.5 * sin time, 2.5 * cos time)

moon :: Behavior Picture
moon = let (x, y) = perimeter
			 in reg (lift0 White) $
						translate (x + 0.35 * sin (5 * time),
												y - 0.35 * cos (5 * time)) $
							shape $ ell 0.1 0.1

earth :: Behavior Picture
earth = reg (lift0 Blue) $
					translate perimeter $
						shape $ ell 0.25 0.25

sun :: Behavior Picture
sun = reg yellow $ shape $ ell 0.5 0.5

orbits :: IO ()
orbits = animateB "Orbits" $ overMany [moon, earth, sun]

