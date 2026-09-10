module Vec3 (Vec3(..), (+^), (-^), (.^), (*^), (/^), mag, cross, unit) where

---------------------------------------------
-- Generic Vec3 type with common functions --
---------------------------------------------

data Vec3 = Vec3 Double Double Double deriving (Show, Eq)

-- Helpers --

pairwise :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
pairwise fn (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (fn x1 x2) (fn y1 y2) (fn z1 z2)

l1 :: Vec3 -> Double
l1 (Vec3 x y z) = x + y + z

-- Functions --

-- Addition
(+^) :: Vec3 -> Vec3 -> Vec3
(+^) = pairwise (+)

-- Subtraction
(-^) :: Vec3 -> Vec3 -> Vec3
(-^) = pairwise (-)

-- Dot Product
(.^) :: Vec3 -> Vec3 -> Double
x .^ y = l1 (pairwise (*) x y)

-- Scalar Multiplication
(*^) :: Vec3 -> Double -> Vec3
(Vec3 x y z) *^ s = Vec3 (s * x) (s * y) (s * z)

-- Scalar Division
(/^) :: Vec3 -> Double -> Vec3
vec /^ s = vec *^ (1/s)

-- Magnitude
mag :: Vec3 -> Double
mag v = sqrt (l1 (pairwise (*) v v))

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3
  ((y1 * z2) - (z1 * y2))
  ((z1 * x2) - (x1 * z2))
  ((x1 * y2) - (y1 * x2))

unit :: Vec3 -> Vec3
unit v = let m = mag v in
  if m == 0 then (Vec3 0 0 0) else v /^ m

infixl 6 +^, -^
infixl 7 *^, /^
infixl 7 .^
