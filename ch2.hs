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


{- 2.3
Prove the following property:
area (Rectangle s1 s2)
    => area (Polygon [(0,0), (s1,0), (s1,s2), (0,s2)])

Noop
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

-- Checks: noncoincident vertices and check that all the crossproducts of 3 neighbouring vertices are positive
convex :: Shape -> Bool
convex (Polygon (v1:v2:vs))   = checkVertices (v1:v2:vs)
    where first = [v1,v2]
          checkVertices :: [Vertex] -> Bool
          checkVertices (v:[]) = nonCoincident (v:first) && positiveCrossProducts (v:first) -- first elems to wraparound
          checkVertices vs'    = nonCoincident vs' && positiveCrossProducts vs'
convex (Rectangle a b)         = True
convex (RtTriangle a b)        = True
convex (Ellipse r1 r2)         = True

-- Checks that non of the vertices does coincide
nonCoincident :: [Vertex] -> Bool
nonCoincident [] = True
nonCoincident (v:vs) = unique v vs && nonCoincident vs
    where unique :: Vertex -> [Vertex] -> Bool
          unique (x1,y1) ((x2,y2):vs')
            = not (x1 == x2 && y1 == y2) && unique (x1,y1) vs' -- not (!)

-- The crossproduct between all adjacent edges
positiveCrossProducts :: [Vertex] -> Bool
positiveCrossProducts (_:_:[]) = True
positiveCrossProducts ((x1, y1):(x2, y2):(x3, y3):vs)
    = let cp = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2) -- crossproduct
      in (cp > 0.0) && positiveCrossProducts ((x2, y2):(x3, y3):vs)

{- 2.5
Write a Haskell function to compute polygonal areas with help of the trapesoid interpretation.

Idea for algorithm: http://www.geocomputation.org/1999/076/gc_076.htm
-}

-- calculates all trapesoids in the polygon measured against the x-axis
trapesoidAreas :: Shape -> Float
trapesoidAreas (Polygon ((x1,y1):vs)) = sum $ trapesoids ((x1,y1):vs) -- remember first position for wraparound
    where
        trapesoids :: [Vertex] -> [Float]
        trapesoids ((x4,y4):[])
            = (x1 - x1) * (y4 - y1) * 0.5 : []
        trapesoids ((x2, y2):(x3, y3):vs')
            = (x3 - x2) * (y2 - y3) * 0.5 : trapesoids ((x3, y3):vs')
