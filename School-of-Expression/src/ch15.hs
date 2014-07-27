module Ch15 where

import Region
import Picture
import qualified SOE
import Fal

{- 15.1
Extend paddleball. Skipped
-}

{- 15.2
Simulate a bouncing ball
-}

bouncingBall :: Behavior Picture
bouncingBall = box `over` ball

box :: Behavior Picture
box = upper `over` left `over` right `over` lower
	where
		upper = paint blue (translate ( 0,1.7) (rec 4.4 0.05))
		left  = paint blue (translate (-2.2,0) (rec 0.05 3.4))
		right = paint blue (translate ( 2.2,0) (rec 0.05 3.4))
		lower = paint blue (translate ( 0,-1.7) (rec 4.4 0.05))


ball :: Behavior Picture
ball = paint yellow (translate (x,y) (ell 0.2 0.2))
	where
		g        = -5
		(x0, y0) = (-2,1.7)
		x        = x0 + integral xvel
		y        = y0 + integral yvel
		xvel     = integral g `switch` (xbounce `snapshot_` xvel =>>
									(\v -> lift0 (v * (-0.93))))
		yvel     = integral g `switch` (ybounce `snapshot_` yvel =>>
									(\v -> lift0 (v * (-0.93)) + integral g))
		xbounce  = when (x <* -2) .|. when (x >* 2)
		ybounce  = when (y <* -1.5) .|. when (y >* 1.5)

{- 15.3
Create an arcade game (Space invaders)
This one was really hard, only got a spaceship shooting ready.
-}

spaceinvaders :: Behavior Picture
spaceinvaders = screen `over` spaceship `over` rocket -- `over` enemies

screen = walls

spaceship :: Behavior Picture
spaceship
	= let
			vs = [(-0.1,-0.1), (0,0.1), (0.1,-0.1)]
			x  = fst mouse
		in paint red (translate (x, -1.7) (tri vs))

tri :: [Vertex] -> Behavior Region
tri vs = shape (lift0 (Polygon vs))

rocket :: Behavior Picture
rocket = paint white (translate (x,y) (rec 0.03 0.04))
	where
		x   = fst mouse `untilB` lbp `snapshot` x =>> (\(_,old) -> lift0 old)
		y	  = (-1.6) `untilB` lbp ->> (-1.6) `stepAccum` Event (\(_,ts) -> aux ts)
		aux = map (\_ -> Just (+0.05))

enemies :: Behavior Picture
enemies = undefined

explosion :: Event Vertex
explosion = undefined









