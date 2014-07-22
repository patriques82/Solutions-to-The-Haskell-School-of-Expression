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

-- Result of implementing spec 1,2,3 (not 4):

sun :: Behavior Picture
sun = reg yellow $ shape $ ell 0.7 0.7

earth :: Behavior Picture
earth = reg (lift0 Blue) $ translate earthsOrbit $
														shape $ ell 0.25 0.25

moon :: Behavior Picture
moon = reg (lift0 White) $ translate moonsOrbit $
												 		shape $ ell 0.1 0.1

earthsOrbit :: (Behavior Time, Behavior Time)
earthsOrbit = ((14*pi/16) * sin time,
							 (3*pi/16) * cos time)

moonsOrbit :: (Behavior Time, Behavior Time)
moonsOrbit
	= let (x, y) = earthsOrbit
		in (x + (4*pi/16) * cos (5 * time),
				y - (pi/16) * sin (5 * time))

(<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(<*) = lift2 (<)

planetOrder :: Behavior Picture
planetOrder = cond (cos time <* 0)       -- if earths y-pos is under suns
								(lift2 over xs sun)      -- then sun behind earth
								(lift2 over sun xs)      -- else earth behind sun
	where xs = cond (cos time <* (cos time - (sin (5 * time))))
								(lift2 over earth moon)  -- moon behind earth
								(lift2 over moon earth)  -- earth behind moon

planets' :: IO ()
planets' = animateB "Orbits" planetOrder

{- 13.3
Build a clock
-}

arm :: Float -> Float -> Behavior Region
arm len tspec
	= let inner_x t = 0.05 * sin (t / tspec)
	      inner_y t = 0.05 * cos (t / tspec)
	      outer_X t = len * sin (t / tspec)
	      outer_Y t = len * cos (t / tspec)
	      delta_x t = 0.025 * cos (t * 0.01)
	      delta_y t = 0.025 * sin (t * 0.01)
	  in Beh (\t -> Shape $ Polygon [(inner_x t + delta_x t, inner_y t + delta_y t),
	                                 (outer_X t + delta_x t, outer_Y t + delta_y t),
	                                 (outer_X t - delta_x t, outer_Y t - delta_y t),
	                                 (inner_x t - delta_x t, inner_y t - delta_y t)])

sec = 2 * pi

second :: Behavior Picture
second = reg (lift0 Black) $ arm 1.8 sec

minute :: Behavior Picture
minute = reg (lift0 Black) $ arm 1.8 (60*sec)

hour :: Behavior Picture
hour = reg (lift0 Black) $ arm 1.4 (60*60*sec)

background :: Behavior Picture
background = reg (lift0 White) $ shape $ ell 1.9 1.9

clock :: IO ()
clock = animateB "Clock" $ overMany [second, minute, hour, background]


{- 13.4 Skipped -}

{- 13.5 Skipped -}

{- 13.6 Skipped -}










