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
       loop' w One False (0,0) (pictToList p)

loop' :: Window -> Bool -> Point -> [(Color, Region)] -> IO ()
loop' w drag old (r:regs)
	= do clearWindow w
			 sequence_ $                                                    -- draw current pictures
				 map (uncurry $ drawRegionInWindow w) (reverse (r:regs))
			 e <- getWindowEvent w                                          -- see whats going on
			 case e of
				 Button new _ _  ->
					 if drag
						 then loop' w False new (r:regs)                          -- user dropped region
						 else let regs' = bringToFront new (r:regs)               -- user started to drag region
									in loop' w True new regs'
				 MouseMove new   ->
					 if drag
						 then let r' = moveFromTo old new r                       -- user is dragging region
									in loop' w drag new (r':regs)
						 else loop' w drag new (r:regs)                           -- just normal mouse moves

-- Brings the clicked region to front of stack
bringToFront :: Point -> [(Color, Region)] -> [(Color, Region)]
bringToFront (x, y) regs
  = let aux (_, r) = r `containsR` (pixelToInch (x - xWin2),
                                    pixelToInch (yWin2 - y))
    in case (break aux regs) of
      (xs,[])          -> xs
      (top, (hit:bot)) -> hit : (top ++ bot)

-- Calculates the distance the user has dragged from previous loop and
-- translates the new distance on the region. (Currently handles only
-- Translate regions)
moveFromTo :: Point -> Point -> (Color, Region) -> (Color, Region)
moveFromTo (x1, y1) (x2, y2) (c, r)
  = let dx = pixelToInch $ x2 - x1
        dy = pixelToInch $ y1 - y2
    in case r of
      Translate (x, y) s -> (c,  Translate ((x + dx), (y + dy)) s)   -- Translate vector shape

main = dragAndDrop "Draggable circles" crossingCircles

crossingCircles = foldr Over EmptyPic cs'
    where cs = [Translate (x, 0) (Shape (circle 1.5)) | x <- [-1,0,1]]  -- Regions
          cs' = map (Region Yellow) cs                                  -- Pictures


