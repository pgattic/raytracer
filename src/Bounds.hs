module Bounds (Bounds(..), unionBounds, hitBounds) where

import Vec3
import Ray
import Interval

data Bounds = Bounds Vec3 Vec3 deriving (Show, Eq)

unionBounds :: Bounds -> Bounds -> Bounds
unionBounds (Bounds (Vec3 a b c) (Vec3 d e f)) (Bounds (Vec3 g h i) (Vec3 j k l)) =
  Bounds (Vec3 (min a g) (min b h) (min c i)) (Vec3 (max d j) (max e k) (max f l))

hitBounds :: Interval -> Ray -> Bounds -> Bool
hitBounds (Interval low high) (Ray (Vec3 ox oy oz) (Vec3 dx dy dz))
          (Bounds (Vec3 ax ay az) (Vec3 bx by bz)) =
  slabs low high [(ox,dx,ax,bx),(oy,dy,ay,by),(oz,dz,az,bz)]
  where
    slabs lo hi [] = lo <= hi
    slabs lo hi ((o,d,a,b):rest)
      | d == 0 = o >= a && o <= b && slabs lo hi rest
      | otherwise =
          let t0 = (a-o)/d
              t1 = (b-o)/d
              nextLo = max lo (min t0 t1)
              nextHi = min hi (max t0 t1)
          in nextLo <= nextHi && slabs nextLo nextHi rest
