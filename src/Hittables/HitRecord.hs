module Hittables.HitRecord (HitRecord(..), createHitRecord ) where

import Point3
import Vec3 (Vec3, (.^), (*^))
import Ray
import Hittables.Material

data HitRecord = HitRecord {
  point :: Point3,
  normal :: Vec3,
  t :: Double,
  frontFace :: Bool,
  material :: Material
}

createHitRecord :: Ray -> Point3 -> Vec3 -> Double -> Material -> HitRecord
createHitRecord ray pt outNorm rayT mat = let
    ff = (direction ray) .^ outNorm < 0;
  in HitRecord {
    point = pt,
    normal = if ff then outNorm else outNorm *^ (-1),
    t = rayT,
    frontFace = ff,
    material = mat
  }
