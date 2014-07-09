module Shape (Shape (...)
             , Radius, Side, Vertex
             , square, circle, distBetween, area
             ) where

-- Book

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
        angle  = (pi * 2) / fromIntegral n     -- angle between each vertex
        radius = (0.5 * s) / sin (0.5 * angle) -- radius of the surrounding circle
        vertices :: Int -> [Vertex]            -- recursive helper function
        vertices 0  = []
        vertices nr = (x, y) : vertices (nr - 1)
            where
                -- increment angle with nr for each new vertex
                x = cos (angle * fromIntegral nr) * radius
                y = sin (angle * fromIntegral nr) * radius

-- Book

area :: Shape -> Float
area (Rectangle s1 s2)  = s1 * s2
area (RtTriangle s1 s2) = s1 * s2 / 2
area (Ellipse r1 r2)    = pi * r1 * r2
area (Polygon (v1:vs))  = polyArea vs
    where
        polyArea :: [Vertex] -> Float
        polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea (v3:vs')
        polyArea _           = 0.0
area _                  = 0.0

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3
    = let a = distBetween v1 v2
          b = distBetween v2 v3
          c = distBetween v3 v1
          s = 0.5 * (a + b + c)
      in sqrt (s * (s - a) * (s - b) * (s - c)) -- Herons formula

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2) -- Pythagorean theorem

{- 2.3
Prove the following property:
area (Rectangle s1 s2)
    => area (Polygon [(0,0), (s1,0), (s1,s2), (0,s2)])
-}

{- 2.4
Define a function "convex :: Shape -> Bool" that determines
whether or not its agument is a convex shape (athough we are mainly
intereste in the convexity of polygons, you might as well define it
for each kind of shape.

Property of a convex polygon:
A vertex V of a polygon is a reflex vertex if its internal angle is strictly greater than pi.
Otherwise the vertex is called convex.

Idea for algorithm: http://debian.fmi.uni-sofia.bg/~sergei/cgsr/docs/clockwise.htm
-}

convex :: Shape -> Bool
convex (Polygon vs@(v1:v2:vs'))   = nonCoincident vs && all (> 0) (crossProducts (vs ++ [v1, v2]))    -- makes use of filter function
convex (Rectangle a b)            = True
convex (RtTriangle a b)           = True
convex (Ellipse r1 r2)            = True

-- Checks that non of the vertices does not coincide
nonCoincident :: [Vertex] -> Bool
nonCoincident [] = True
nonCoincident vs
    = let top  = head vs
          rest = tail vs
      in all (\(x,y) -> x /= fst top && y /= snd top) rest && nonCoincident rest

-- Checks that all angles are counterclockwise (positive crossproduct between all adjacent edges)
crossProducts :: [Vertex] -> [Float]
crossProducts (_:_:[]) = []
crossProducts ((x1, y1):(x2, y2):(x3, y3):vs)
    = let cp            = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)
      in cp:crossProducts ((x2, y2):(x3, y3):vs)

{- 2.5
Write a Haskell function to compute polygonal areas with help of the trapesoid interpretation.

Idea for algorithm: http://www.geocomputation.org/1999/076/gc_076.htm
still figuring it out...
-}

-- calculates all trapesoids in the polygon measured against the x-axis
trapesoidAreas :: Shape -> Float
trapesoidAreas (Polygon vs) = sum $ trapesoids (vs ++ [head vs])
    where
        trapesoids :: [Vertex] -> [Float]
        trapesoids (_:[]) = []
        trapesoids ((x1, y1):(x2, y2):vs')
            = (x2 - x1) * (y1 - y2) * 0.5 : trapesoids ((x2, y2):vs')
