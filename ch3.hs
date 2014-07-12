module SimpleGraphics where

import SOE

{- 3.1
Define putStr and getLine recursively
-}

putStr'' :: String -> IO ()
putStr'' [] = return () -- noop (base case)
putStr'' (c:cs) = do
    putChar c
    putStr'' cs

getLine' :: IO String
getLine' = do
    c <- getChar
    if c == '\n'
      then return ""
      else do cs <- getLine
              return (c:cs)


{- 3.2
draw a recursive snowflake
-}

-- All angles between corners and midpoint are 60).
circumferencePoints :: Double -> Double -> Double -> Point -> [Point]
circumferencePoints r a da (x, y)
    = if da < (pi * 2)                                             -- only one rotation (360)
        then let dx = round (cos da * r)                           -- now convert dx and dy to Int as a final step (round)
                 dy = round (sin da * r)
                 p = (x + dx, y + dy)
             in p : circumferencePoints r a (da + a) (x, y)        -- move counterclockwise
        else []

-- David corners on the circumference of a circle.
-- This function and circumferencePoints requires floating precision for calculations.
drawDavid :: Window -> Color -> Point -> Double -> IO [Point]
drawDavid w c mid side = do
    let angle = 2 * pi / 3                                         -- 120°
        radius = sqrt (side^2 - (0.5 * side)^2) / 2
        (v1:v1s) = circumferencePoints radius angle (pi/6) mid     -- starting angles (30 and 90)
        (v2:v2s) = circumferencePoints radius angle (pi/2) mid
    drawInWindow w (withColor c (polygon (append (v1:v1s) [v1])))
    drawInWindow w (withColor c (polygon (append (v2:v2s) [v2])))
    return (append (v1:v1s) (v2:v2s))

snowFlake :: Window -> [Color] -> Double -> Point -> IO ()
snowFlake w [] side mid = snowFlake w colors side mid
snowFlake w (c:cs) side mid
    = if side <= minSize
        then return ()                                             -- base case
        else do                                                    -- recursive step
            corners <- drawDavid w c mid side                      -- corners of david
            mapM_ (snowFlake w cs (side/3)) corners                -- mapM_ : applies a function on all elements of list

append :: [Point] -> [Point] -> [Point]
append [] vs      = vs
append (v:vs) vs' = v : append vs vs'

colors :: [Color]
colors = [Blue, Green, Cyan, Red, Magenta, Yellow, White] -- all except Black

minSize :: Double
minSize = 8.0

main4 :: IO ()
main4 = runGraphics (
        do w <- openWindow "Snowflake Fractal" (400, 400)
           snowFlake w colors 400 (300,300)
           k <- getKey w
           closeWindow w
        )
