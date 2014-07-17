module Ch10 where

import Picture

{- 10.1
Use draw to draw the above pictures. And also try drawing the "five
circles" in chapter 8.
-}

fiveCircles = Region Yellow reg
    where reg = foldr Union c c4
          (c:cs) = [Translate (x, 0) (Shape (circle 0.5)) | x <- [0,1..4]]

drawAllPics :: IO ()
drawAllPics = draw "drawAllPics" fiveCircles

{- 10.2 Skipped -}
