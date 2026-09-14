module Hittables.HitRecord (HitRecord(..), createHitRecord ) where

import Point3
import Vec3 (Vec3, (.^), (*^))
import Ray

data HitRecord = HitRecord {
  point :: Point3,
  normal :: Vec3,
  t :: Double,
  frontFace :: Bool
}

createHitRecord :: Ray -> Point3 -> Vec3 -> Double -> HitRecord
createHitRecord ray pt outNorm rayT = let
    ff = (direction ray) .^ outNorm < 0;
  in HitRecord {
    point = pt,
    normal = if ff then outNorm else outNorm *^ (-1),
    t = rayT,
    frontFace = ff
  }
