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
spaceinvaders = screen `over` spaceship `over` rockets -- `over` enemies

screen = walls

spaceship :: Behavior Picture
spaceship =
	let
		vs = [(-0.1,-0.1), (0,0.1), (0.1,-0.1)]
		x  = fst mouse
	in paint red (translate (x, -1.7) (tri vs))

tri :: [Vertex] -> Behavior Region
tri vs = shape (lift0 (Polygon vs))

ship_x :: Behavior Float
ship_x = Behavior (\uts -> loop uts (fs uts))
	where
		Behavior fs = spaceship
		loop ((_:us),(_:ts)) ((Region _ (Translate (x,_) _)):bs) = x : loop (us,ts) bs

append :: Event () -> Behavior Picture -> Behavior Picture
append (Event e) ~(Behavior ps)
	= memoB $ Behavior (\uts@(us,ts) -> loop us ts (e uts) (ps uts))
     where loop (_:us) (_:ts) ~(e:es) ~(b:bs) =
             b : case e of
                   Nothing -> loop us ts es bs
                   Just () -> loop us ts es (zipWith' Over (ps (us,ts)) bs)

rockets :: Behavior Picture
rockets = append lbp rocket

rocket :: Behavior Picture
rocket  = paint white (translate (x, y) (rec 0.03 0.04))
	where
		x   = ship_x `untilB` lbp `snapshot_` x =>> constB
		y	= (-1.6) `untilB` lbp ->> (-1.6) `stepAccum` Event (\(_,ts) -> aux ts)
		aux = map (\_ -> Just (+0.04))

{- lbp_p :: Event SOE.Point-}
{- lbp_p = Event (\(uas,_) -> map getlbp uas)-}
{-     where-}
{-         getlbp (Just (SOE.Button p True True)) = Just p-}
{-         getlbp _                               = Nothing-}

{- int2Float :: Int -> Float-}
{- int2Float = fromInteger . toInteger-}

enemies :: Behavior Picture
enemies = undefined

explosion :: Event Vertex
explosion = undefined









