module Ch13 where

import Region
import Animation

{- 13.1
Create instances of Regions and Shapes in the class Combine.
Can you think of reasonable instances of String and Float?
-}

instance Combine Region where
	empty = Empty
	over  = Union

instance Combine Shape where
	empty = Polygon []
	over  = 
