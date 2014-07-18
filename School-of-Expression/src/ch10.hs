module Ch10 where

import Picture
import Draw
import Region
import SOE hiding (Region)
import qualified SOE as G (Region)

{- 10.1
Use draw to draw the above pictures. And also try drawing the "five
circles" in chapter 8.
-}

fiveCircles = Region Yellow reg
    where reg = foldr Union c cs
          (c:cs) = [Translate (x, 0) (Shape (circle 0.5)) | x <- [0,1..4]]

drawAllPics :: IO ()
drawAllPics = draw "drawAllPics" fiveCircles

{- 10.2 Skipped -}

{- 10.3 Skipped -}

{- 10.4
Implement drag and drop
-}

xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

pictToList :: Picture -> [(Color,Region)]
pictToList  EmptyPic      = []
pictToList (Region c r)   = [(c,r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2

dragAndDrop :: String -> Picture -> IO ()
dragAndDrop s p
  = runGraphics $
    do w <- openWindow s (xWin, yWin)
       loop' w False (0,0) (pictToList p)

loop' :: Window -> Bool -> Point -> [(Color, Region)] -> IO ()
loop' w dragging old (r:regs)
  = do clearWindow w
       sequence_                                                        -- draw current pictures
         (map (uncurry (drawRegionInWindow w))
            (reverse (r:regs)))
       e <- getWindowEvent w                                            -- see whats going on
       case e of
         Button pt l down -> start dragging pt
         MouseMove pt     -> moveIf dragging pt
    where start d new
            | d          = loop' w False new (r:regs)                   -- stop dragging
            | otherwise  = loop' w True new (bringToFront new (r:regs)) -- start dragging
          moveIf d new
            | d          = loop' w d new ((moveFrom old new r):regs)    -- move from old to new pos
            | otherwise  = loop' w d new (r:regs)                       -- just moving

bringToFront :: Point -> [(Color, Region)] -> [(Color, Region)]
bringToFront (x, y) regs
  = let aux (_,r) = r `containsR` (pixelToInch (x-xWin2),
                                   pixelToInch (yWin2-y))
    in case (break aux regs) of
      (xs,[])       -> xs
      (top,hit:bot) -> hit : (top ++ bot)

moveFrom :: Point -> Point -> (Color, Region) -> (Color, Region)
moveFrom (x1, y1) (x2, y2) (c, r)
  = let dx = pixelToInch $ x2 - x1
        dy = pixelToInch $ y1 - y2
    in case r of
      Translate (x, y) s -> (c,  Translate ((x + dx), (y + dy)) s)

main = dragAndDrop "Draggable circles" circles

circles = foldr Over EmptyPic cs'
    where cs = [Translate (x, 0) (Shape (circle 1.5)) | x <- [-1,0,1]]  -- Regions
          cs' = map (Region Yellow) cs                                  -- Pictures


