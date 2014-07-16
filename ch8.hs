module Ch8 where

{-import SOE-}
import Ch2
import Ch5


{- 8.1
Define a function that creates five circles by intersecting a region
containing an infinite sequence of circles with a suitable sized and
positioned rectangle.
-}

fiveCircles :: Region
fiveCircles = Translate (5.0,0.0) (Shape (Rectangle 10 2)) `Intersect` reg
    where reg = foldr Union c' cs'
          (c':cs') = [Translate (x, 0) c | c <- cs, x <- [0,2..]]
          cs = map Shape (repeat (circle 1))

{- 8.2
Modify the previous definitions of area and perimeter so that negative
arguments work properly (the area and perimeter should always be positive).
-}

area' :: Shape -> Float
area' (Rectangle s1 s2) = abs (s1 * s2)
area' (RtTriangle s1 s2) = abs (s1 * s2 * 0.5)
area' (Ellipse r1 r2) = abs (pi * r1 * r2)
area' (Polygon vs) = area (Polygon vs)                  -- makes use of old def

perimeter' :: Shape -> Float
perimeter' (Rectangle s1 s2)  = (abs s1 + abs s2) * 2
perimeter' (RtTriangle s1 s2) = abs s1 + abs s2 + distBetween (s1,0) (0,s2)
perimeter' (Polygon vs)       = foldr (+) 0 (sides vs)
perimeter' (Ellipse r1 r2)
    | abs r1 > abs r2 = ellipsePerim r1' r2'
    | otherwise       = ellipsePerim r2' r1'
        where (r1', r2') = (abs r1, abs r2)
              ellipsePerim r1 r2
                = let e       = sqrt (r1^2 - r2^2) / r1
                      s       = scanl aux (0.25 * e^2) [2..]
                      aux s i = nextEl e s i
                      test x  = x > epsilon
                      sSum    = foldl (+) 0 (takeWhile test s)
                  in 2 * r1 * pi * (1 - sSum)

sides :: [Vertex] -> [Side]
sides vs = zipWith distBetween vs (tail vs ++ [head vs])

epsilon = 0.0001 :: Float

nextEl :: Float -> Float -> Float -> Float
nextEl e s i = s * (2*i-1) * (2*i-3) * (e^2) / (4*i^2)  -- where i >= 2

{- 8.3
Define a function:
    annulus :: Radius -> Radius -> Region
that creates a annulus, or "donut" whose first argument is the inner radius
and the second is the outer.
-}

annulus :: Radius -> Radius -> Region
annulus r1 r2 = Shape (circle r2) `Intersect` Complement (Shape (circle r1))

{- 8.4
The definition of containS for polygons requires that the list of vertices
be in clockwise order. Redefine it so that the list could be in either order.
-}

type Coordinate = (Float, Float)
type Ray = (Coordinate, Coordinate)

containsS' :: Shape -> Coordinate -> Bool
(Polygon pts) `containsS'` p
    = let leftOfList = map isLeftOfp (zip pts (tail pts ++ [head pts]))
          isLeftOfp r = isLeftOf p r
          rightOfList = map isRightOfp (zip pts (tail pts ++ [head pts]))
          isRightOfp r = isRightOf p r
      in and leftOfList || and rightOfList

isLeftOf :: Coordinate -> Ray -> Bool
p `isLeftOf` r = isSideOf (>=) p r

isRightOf :: Coordinate -> Ray -> Bool
p `isRightOf` r = isSideOf (<=) p r

isSideOf :: (Float -> Float -> Bool) -> Coordinate -> Ray -> Bool
isSideOf op (px, py) ((ax, ay), (bx, by))
    = let (s, t) = (px - ax, py - ay)
          (u, v) = (px - bx, py - by)
      in op (s * v) (t * u)

{- 8.5
Add a constructor HalfPlane to the Region data type such that HalfPlane p1 p2
denotes the infinite region, or half plane, to the left pf the line formed by
the two points p1 and p2. Extend the definition of containsR to include this
constructor.
-}


data Region = Shape Shape               -- primitive shape
            | Translate Vector Region   -- translated region
            | Scale      Vector Region  -- scaled region
            | Complement Region         -- inverse of region
            | Region `Union` Region     -- union of regions
            | Region `Intersect` Region -- intersection of regions
            | Region `Xor` Region       -- XOR of regions
            | HalfPlane Vector Vector   -- infinite region to left of the line
            | Empty                     -- empty region
     deriving Show

type Vector = (Float,Float)

containsR :: Region -> Coordinate -> Bool
Empty `containsR` p                   = False
(Shape s) `containsR` p               = s `containsS'` p
(Translate (u,v) r) `containsR` (x,y) = r `containsR` (x-u,y-v)
(Scale (u,v) r) `containsR` (x,y)     = r `containsR` (x/u,y/v)
(Complement r) `containsR` p          = not (r `containsR` p)
(r1 `Union` r2) `containsR` p         = r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p     = r1 `containsR` p && r2 `containsR` p
(HalfPlane u v) `containsR` p         = p `isLeftOf` (u,v)


{- 8.6
Define a function polygon that realizes a polygon in terms of the HalfPlane, Polygon contructor
and possibly other constructors.
-}

polygon' :: [Vector] -> Region
polygon' (v1:v2:v3:vs) = foldr Intersect (Complement Empty) hps  -- patternmatching on minimum 3 vectors
    where hps = [HalfPlane u v | (u, v) <- zip (v1:v2:v3:vs) ((v2:v3:vs) ++ [v1])]
polygon' _      = Empty

{- 8.7
Define a function:
    flipX :: Region -> Region
that flips a region about the x-axis. Also define a flipY function.
-}

flipX, flipY :: Region -> Region

flipX (Translate x r)               = Translate x (flipX r)
flipX (Scale x r)                   = Scale x (flipX r)
flipX (Complement r)                = Complement (flipX r)
flipX (r1 `Union` r2)               = (flipX r1) `Union` (flipX r2)
flipX (r1 `Intersect` r2)           = (flipX r1) `Intersect` (flipX r2)
flipX (r1 `Xor` r2)                 = (flipX r1) `Xor` (flipX r2)
flipX (HalfPlane (x1, y1) (x2, y2)) = HalfPlane (x2, -y2) (x1, -y1)
flipX (Shape (Polygon vs))          = Shape (Polygon (map (\(x,y) -> (x,-y)) vs))
flipX (Shape (RtTriangle s1 s2))    = Shape (RtTriangle s1 (-s2))
flipX (Shape s)                     = Shape s
flipX Empty                         = Empty

flipY (Translate x r)               = Translate x (flipY r)
flipY (Scale x r)                   = Scale x (flipY r)
flipY (Complement r)                = Complement (flipY r)
flipY (r1 `Union` r2)               = (flipY r1) `Union` (flipY r2)
flipY (r1 `Intersect` r2)           = (flipY r1) `Intersect` (flipY r2)
flipY (r1 `Xor` r2)                 = (flipY r1) `Xor` (flipY r2)
flipY (HalfPlane (x1, y1) (x2, y2)) = HalfPlane (-x2, y2) (-x1, y1)
flipY (Shape (Polygon vs))          = Shape (Polygon (map (\(x,y) -> (-x,y)) vs))
flipY (Shape (RtTriangle s1 s2))    = Shape (RtTriangle (-s1) s2)
flipY (Shape s)                     = Shape s
flipY Empty                         = Empty

{- 8.8

Axiom 3:
r1 `Intersect` (r2 `Union` r3) = (r1 `Intersect` r2) `Union` (r1 `Intersect` r3)
Did this one backwards, Oops!

((r1 `Intersect` r2) `Union` (r1 `Intersect` r3)) `containsR` p
=> (r1 `Intersect` r2) `containsR` p || (r1 `Intersect` r3) `containsR` p
=> r1 `containsR` p && r2 `containsR` p || r1 `containsR` p && r3 `containsR` p
=> r1 `containsR` p && r2 `containsR` p || r3 `containsR` p
=> r1 `containsR` p && (r2 `Union` r3) `containsR` p
=> r1 `Intersect` (r2 `Union` r3) `containsR` p

r1 `Union` (r2 `Intersect` r3) = (r1 `Union` r2) `Intersect` (r1 `Union` r3)

(r1 `Union` (r2 `Intersect` r3)) `containsR` p
=> r1 `containsR` p || r2 `containsR` p && r3 `containsR` p
=> r1 `containsR` p || r2 `containsR` p && r3 `containsR` p || r1 `containsR` p
=> r1 `containsR` p || r2 `containsR` p && r3 `containsR` p || r3 `containsR` p
=> (r1 `Union` r2) `containsR` p && (r3 `Union` r3) `containsR` p
=> (r1 `Union` r2) `Intersect` (r1 `Union` r3) `containsR` p



Axiom 4:
r `Union` Empty = r

(r `Union` Empty) `containsR` p
=> r `containsR` p || Empty `containsR` p
=> r `containsR` p || False
=> r `containsR` p

r `Intersect` univ = r

(r `Intersect` univ) `containsR` p
=> r `containsR` p && univ `containsR` p
=> r `containsR` p && True
=> r `containsR` p



Axiom 5:
r `Union` Complement r = univ

(r `Union` Complement r) `containsR` p
=> r `containsR` p || (Complement r) `containsR` p
=> (r `Union` Complement r) `containsR` p
=> True (for all cases)
=> univ `containsR` p

r `Intersect` Complement r = Empty

(r `Intersect` Complement r) `containsR` p
=> False (for all cases)
=> Empty `containsR` p

-}

{- 8.9
Prove the following theorems using the axioms only:

Axiom 3:
r1 `Intersect` (r2 `Union` r3) = (r1 `Intersect` r2) `Union` (r1 `Intersect` r3)
r1 `Union` (r2 `Intersect` r3) = (r1 `Union` r2) `Intersect` (r1 `Union` r3)
Axiom 4:
r `Union` Empty = r
r `Intersect` univ = r
Axiom 5:
r `Union` Complement r = univ
r `Intersect` Complement r = Empty


r `Intersect` r = r                                                     : Theorem 1
=> (r `Intersect` r) `Union` Empty                                      : Axiom 4
=> (r `Intersect` r) `Union` (r `Intersect` Complement r)               : Axiom 5
=> r `Intersect` (r `Union` Complement r)                               : Axiom 3
=> r `Intersect` univ                                                   : Axiom 5
=> r                                                                    : Axiom 4

r `Union` r = r                                                         : Theorem 2
=> (r `Union` r) `Intersect` univ                                       : Axiom 4
=> (r `Union` r) `Intersect` (r `Union` Complement r)                   : Axiom 5
=> r `Union` (r `Intersect` Complement r)                               : Axiom 3
=> r `Union` Empty                                                      : Axiom 5
=> r                                                                    : Axiom 4

r `Union` univ = univ                                                   : Theorem 3
=> (r `Union` univ) `Intersect` univ                                    : Axiom 4
=> (r `Union` univ) `Intersect` (r `Union` Complement r)                : Axiom 5
=> r `Union` (univ `Intersect` Complement r)                            : Axiom 3
=> r `Union` Complement r                                               : Axiom 4
=> univ                                                                 : Axiom 5

r `Intersect` Empty = Empty                                             : Theorem 4
=> (r `Intersect` Empty) `Union` Empty                                  : Axiom 4
=> (r `Intersect` Empty) `Union` (r `Intersect` Complement r)           : Axiom 5
=> r `Intersect` (Empty `Union` Complement r)                           : Axiom 3
=> r `Intersect` Complement r                                           : Axiom 4
=> Empty                                                                : Axiom 5

r1 `Union` (r1 `Intersect` r2) = r1                                     : Theorem 5
=> (r1 `Intersect` univ) `Union` (r1 `Intersect` r2)                    : Axiom 4
=> r1 `Intersect` (univ `Union` r2)                                     : Axiom 3
=> r1 `Intersect` univ                                                  : Theorem 3
=> r1                                                                   : Axiom 4

r1 `Intersect` (r1 `Union` r2) = r1                                     : Theorem 6
=> (r1 `Union` Empty) `Intersect` (r1 `Union` r2)                       : Axiom 4
=> r1 `Union` (Empty `Intersect` r2)                                    : Axiom 3
=> r1 `Union` Empty                                                     : Theorem 4
=> r1                                                                   : Axiom 4

Complement (Complement r) = r                                           : Theorem 7
let r' = Complement r
=> Complement r' `Intersect` univ                                       : Axiom 4
=> Complement r' `Intersect` (r `Union` r'))                            : Axiom 5
=> (Complement r' `Intersect` r) `Union` (Complement r' `Intersect` r') : Axiom 3
=> (Complement r' `Intersect` r) `Union` Empty                          : Axiom 5
=> (Complement r' `Intersect` r) `Union` (r' `Intersect` r)             : Axiom 5
=> r `Intersect` (Complement r' `Union` r')                             : Axiom 3
=> r `Intersect` univ                                                   : Axiom 5
=> r                                                                    : Axiom 4

Complement Empty = univ                                                 : Theorem 8
=> Complement Empty `Union` Empty                                       : Axiom 4
=> univ                                                                 : Axiom 5

Complement univ = Empty                                                 : Theorem 9
=> Complement univ `Intersect` univ                                     : Axiom 4
=> Empty                                                                : Axiom 5

...

-}

{- 8.10
Prove the following facts:

Rectangele s1 s2 `containsS` p = Rectangle (-s1) s2 `containsS` p

Rectangle s1 s2 `containsS` (x, y)
=> let t1 = s1/2
       t2 = s2/2
   in -t1 <= x && x <= t1 && -t2 <= y && y <= t2
=> -(s1/2) <= x && x <= (s1/2) && -(s2/2) <= y && y <= (s2/2)

Rectangle (-s1) s2 `containsS` (x, y)
=> let t1 = (-s1)/2
       t2 = s2/2
   in -t1 <= x && x <= t1 && -t2 <= y && y <= t2
=> -((-s1)/2) <= x && x <= ((-s1)/2) && -t2 <= y && y <= t2

We see that the only thing that differs between the expressions are
the subexpressions:
    -(s1/2) <= x && x <= (s1/2)
    -((-s1)/2) <= x && x <= ((-s1)/2)

The book claim that:
    (-(s1/2) <= x && x <= (s1/2)) == (-((-s1)/2) <= x && x <= ((-s1)/2))

Which can be disproved by a counter example:
let s1 = 1
in (-(s1/2) <= x && x <= (s1/2)) == (-((-s1)/2) <= x && x <= ((-s1)/2))
=> (-0.5 <= x && x <= 0.5) == (0.5 <= x && x <= -0.5)
=> True (iff False == False)



Ellipse r1 r2 `containsS` p = Ellipse (-r1) r2 `containsS` p

(Ellipse r1 r2) `containsS` (x,y) = (x/r1)^2 + (y/r2)^2 <= 1

(Ellipse r1 r2) `containsS` (x,y)
=> (x/r1)^2 + (y/r2)^2 <= 1

Ellipse (-r1) r2 `containsS` p
=> (x/(-r1))^2 + (y/r2)^2 <= 1

The book claim that:
((x/r1)^2 + (y/r2)^2 <= 1) == ((x/(-r1))^2 + (y/r2)^2 <= 1)

((x/r1)^2 + (y/r2)^2 <= 1) == ((x/(-r1))^2 + (y/r2)^2 <= 1)
=> ((x^2/r1^2) + (y^2/r2^2) <= 1) == (x^2/(-r1)^2) + (y^2/r2^2) <= 1)
=> ((x^2/r1^2) + (y^2/r2^2) <= 1) == (x^2/r1^2) + (y^2/r2^2) <= 1)
=> True

-}

{- 8.11
Define a function to check if a Polygons vertices describe a convex Polygon.
Then rewrite area.
-}

-- From exercise 2.4
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

area'' :: Shape -> Float
area'' (Rectangle s1 s2) = abs (s1 * s2)
area'' (RtTriangle s1 s2) = abs (s1 * s2 * 0.5)
area'' (Ellipse r1 r2) = abs (pi * r1 * r2)
area'' (Polygon vs) = if convex (Polygon vs)
                        then area' (Polygon vs)
                        else error "area'': Polygon not convex"













