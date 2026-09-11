module Hittables.Hittable where

import Vec3 (Vec3)
import Ray (Ray)

data HitRecord = HitRecord {
  point :: Vec3,
  normal :: Vec3,
  t :: Double
}

class Hittable a where hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
