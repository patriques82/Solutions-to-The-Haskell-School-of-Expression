module Ch19 where

import SOE
import Robot

{- 19.1
Prove that the instance of Monad for the type Robot given satisfies
the monad laws.

instance Monad Robot where
  return a
    = Robot (\s _ _ -> return (s,a))
  Robot sf0 >>= f
    = Robot $ \s0 g w -> do
                (s1,a1) <- sf0 s0 g w
                let Robot sf1 = f a1
                (s2,a2) <- sf1 s1 g w
                return (s2,a2)

1.
return x >>= k
=> Robot (\s _ _ -> return (s,x)) >>= k
=> Robot sf0 >>= k                    -- rename
=> Robot $ (\s0 g w -> do
						 (s0,x) <- sf0 s0 g w     -- same state get passed on unchanged
						 let Robot sf1 = k x
						 (s1,a) <- sf1 s0 g w
						 return (s1,a))
=> Robot $ (\s0 g w -> do
						 let Robot sf1 = k x      -- so we can simplify
						 (s1,a) <- sf1 s0 g w
						 return (s1,a))
=> Robot sf1                          -- and simplify
=> k x

2.
Robot sf0 >>= return
=> Robot (\s0 g w -> do
					(s1,a1) <- sf0 s0 g w
					let Robot sf1 = return a1
					(s2,a2) = sf1 s1 g w
					return (s2,a2))
=> Robot (\s0 g w -> do
					(s1,a1) <- sf0 s0 g w
					let Robot (\s _ _ -> return (s,a1)) = return a1 -- expand expression
					(s1,a1) = (\s _ _ -> return (s,a1) s1 g w
					return (s1,a1))
=> Robot (\s0 g w -> do
					(s1,a1) <- sf0 s0 g w
					return (s1,a1))
=> Robot sf0

3.
Robot sf0 >>= (\x -> k x >>= h)
=> Robot (\s0 g w -> do
					 (s1,a1) <- sf0 s0 g w
					 let Robot sf1 = (\x -> k x >>= h) a1
					 (s2,a2) = sf1 s1 g w
					 return (s2,a2))
=> Robot (\s0 g w -> do
					 (s1,a1) <- sf0 s0 g w
					 (s2,a2) = (k a1 >>= h) s1 g w
					 return (s2,a2))
=> Robot (\s0 g w -> do
					 (s1,a1) <- sf0 s0 g w
					 (s2,a2) = sf1 s1 g w >>= h
					 return (s2,a2))
=> Robot (\s0 g w -> do
					 (s1,a1) <- sf0 s0 g w
					 (s2,a2) <- sf1 s1 g w
					 let Robot sf2 = h a2
					 (s3,a3) = sf2 s2 g w
					 return (s3,a3))
=> Robot (\s0 g w -> do
					 (s1,a1) <- sf0 s0 g w
					 let Robot sf1 = k a1
					 (s2,a2) <- sf1 s1 g w
					 return (s2,a2)) >>= h
=> (Robot sf0 >>= k) >>= h

-}

{- 19.2
Define functions in terms of IRL.
-}

repeat :: Robot Bool -> Robot () -> Robot ()
repeat p r = r >> (while p r)

blockedLeft', blockedRight', blockedBehind' :: Robot Bool

blockedLeft' = do
	turnLeft
	b <- blocked
	turnRight
	return b

blockedRight' = do
	turnRight
	b <- blocked
	turnLeft
	return b

blockedBehind' = do
	b <- (turnLeft >> turnLeft >> blocked)
	turnRight
	turnRight
	return b

wallFollowLeft, wallFollowRight :: Robot ()

wallFollowLeft = while blockedLeft' move

wallFollowRight = while blockedRight' move

{- 19.3
Implement the functions as primitives to the IRL.
-}

getPosition :: Robot Point
getPosition = Robot (\s0 _ _ -> do
	let p = position s0
	return (s0, p))

goToPosition :: Point -> Robot ()
goToPosition p = Robot (\s0 _ _ ->
	return (s0 {position = p}, ()))

blockedLeft'', blockedRight'', blockedBehind'' :: Robot Bool

blockedLeft'' = Robot (\s0 g _ -> do
	let dirs = g `at` (position s0)
	    ldir = left (facing s0)
	return (s0, ldir `elem` dirs))

blockedRight'' = Robot (\s0 g _ -> do
	let dirs = g `at` (position s0)
	    rdir = right (facing s0)
	return (s0, rdir `elem` dirs))

blockedBehind'' = Robot (\s0 g _ -> do
	let dirs = g `at` (position s0)
	    rdir = behind (facing s0)
	return (s0, rdir `elem` dirs))

behind :: Direction -> Direction
behind d = toEnum (pred (fromEnum d) `mod` 2)

{- 19.4
Write IRL programs
-}

-- 1.
-- Scatters the coins on top of grid
spiral' :: Robot ()
spiral' = penDown >> loop 1
 where loop n =
         let twice = do turnRight
                        moven n
                        turnRight
                        moven n
         in cond blocked
              (twice >> turnRight >> moven n)
              (twice >> loop (n+1))

test = runRobot spiral' coinsS g0

coinsS  :: RobotState
coinsS = s0 { treasure = [(x,y) | x <- [-13,-11 .. 1], y <- [9,11 .. 15]] }





