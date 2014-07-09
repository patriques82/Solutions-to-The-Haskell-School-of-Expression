module Shape where

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
    deriving Show

type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

square s = Rectangle s s

circle r = Ellipse r r

{- 2.1
Define functions "rectangle" and "rtTriangle" in terms of
Polygon.
-}

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = Polygon [(0, 0), (s1, 0), (s1, s2), (0, s2)]

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [(0, 0), (s1, s2), (0, s2)]

{- 2.2
Define a function "regularPolygon :: Int -> Side -> Shape"
such that "regularPolygon n s" is a regular polygon with n sides, each of
length s. (Hint: Consider using some of Haskell´s trigonometric functions,
such as "sin :: Float -> Float", "cos :: Float -> Float", and "tan :: Float -> Float".)
-}

-- This function considers every polygon defined within the borders of a circle
-- where each vertex touches the circles circumference, the center of it is the origin
regularPolygon :: Int -> Side -> Shape
regularPolygon n s = Polygon $ vertices n
    where
        angle  = (pi * 2) / fromIntegral n
        radius = (1 / sin (angle / 2)) * (s / 2)
        vertices nr | nr > 0    = (x, y) : vertices (nr - 1) -- recursive definition
                    | otherwise = []
            where
                -- increment angle with nr for each new vertex
                x = cos (angle * fromIntegral nr) * radius
                y = sin (angle * fromIntegral nr) * radius

regularPolygon' :: Int -> Side -> Shape
regularPolygon' n s
    = let angleinc = pi * 2 / fromIntegral n
          radius = s * sin ((pi - angleinc) / 2) / sin angleinc
          regularVerts 0 _     = []
          regularVerts n angle = (radius * cos angle, radius * sin angle) : regularVerts (n-1) (angle + angleinc)
      in Polygon (regularVerts n 0)

