module Ch1 where

simple a b c = a * (b + c)

{- 1.1
Write out all the steps in the calculation of the value of
simple (simple 2 3 4) 5 6

simple (simple 2 3 4) 5 6
    => (simple 2 3 4) * (5 + 6)
    => (2 * (3 + 4)) * (5 + 6)
    => (2 * 7) * (5 + 6)
    => 14 * (5 + 6)
    => 14 * 11
    => 154
-}

{- 1.2
Prove by calculation that simple (a - b) a b => a^2 - b^2

simple (a - b) a b
    => (a - b) * (a + b)
    => (a + (-b)) * (a + b)
    => (a^2 + ab) + (-ba + (-b^2))
    => a^2 - b^2
-}

{- 1.3
Identify the well-typed expressions in the following and,
for each, give its proper type:

[(2,3), (4,5)]
[(2,3), (4,5)] :: Num t => [(t, t)]

['z', 42]
['z', 42] :: error "No instance for (Num Char) arising from the literal `42'"

('z', -42)
('z', -42) :: Num t => (Char, t)

simple 'a' 'b' 'c'
simple 'a' 'b' 'c' :: error

(simple 1 2 3, simple)
(simple 1 2 3, simple) :: Num t => (t, (t -> t -> t -> t))
-}

circleArea :: Float -> Float
circleArea r = pi * (r ** 2) -- power of 2

totalArea :: Float -> Float -> Float -> Float
totalArea r1 r2 r3 = circleArea r1 + circleArea r2 + circleArea r3

listSum :: [Float] -> Float
listSum []     = 0
listSum (x:xs) = x + listSum xs

totalArea' :: Float -> Float -> Float -> Float
totalArea' r1 r2 r3 = listSum [circleArea r1, circleArea r2, circleArea r3]










