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
    = let dx = round (cos da * r) -- now convert to Int as a final step
          dy = round (sin da * r)
          p = (x + dx, y + dy)
      in p : circumferencePoints r a (da + a) (x, y) -- move counterclockwise

-- David corners on the circumference of a circle.
-- This function and circumferencePoints requires floating precision for calculations.
david :: Point -> Double -> [Point]
david mid@(x,y) s
    = let radius = sqrt (s^2 - (0.5 * s)^2) / 2
          angle  = 2 * pi / 6 -- 60°
          delta  = angle / 2  -- 30° (starting angle for first point)
      in take 6 $ circumferencePoints radius angle delta mid

drawDavid :: Window -> Color -> Point -> Int -> IO [Point]
drawDavid w c mid size = do
    let corners = david mid (fromIntegral size) -- fromIntegral size to convert to Double
        indexed = zip [1..] corners
        tri1 = [snd x | x <- indexed, odd $ fst x]
        tri2 = [snd x | x <- indexed, even $ fst x]
    drawInWindow w (withColor c (polygon $ tri1 ++ [head tri1]))
    drawInWindow w (withColor c (polygon $ tri2 ++ [head tri2]))
    return corners

snowFlake :: Window -> [Color] -> Int -> Point -> IO ()
snowFlake w [] size mid = snowFlake w colors size mid
snowFlake w (c:cs) size mid
    = if size <= minSize
        then return () -- base case
        else do -- recursive step
            corners <- drawDavid w c mid size -- list of points
            mapM_ (snowFlake w cs (size `div` 3)) corners
            -- since we are in a monad context and our function represents a monadic value
            -- we must do mapM_ instead of map

colors :: [Color]
colors = [Blue, Green, Cyan, Red, Magenta, Yellow, White] -- all except Black

minSize :: Int
minSize = 8

close :: Window -> IO ()
close w = do
    k <- getKey w
    closeWindow w

main4 :: IO ()
main4 = runGraphics (
        do w <- openWindow "Snowflake Fractal" (400, 400)
           snowFlake w colors 400 (300,300)
           close w
        )
