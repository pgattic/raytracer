module Hittables.Sphere where

import Vec3 (Vec3, mag_squared, (-^), (.^), (/^))
import Hittables.Hittable (Hittable(..), HitRecord(..))
import Ray (Ray(direction, origin), at)

data Sphere = Sphere {
  center :: Vec3,
  radius :: Double
}

nearestRoot :: Double -> Double -> Double -> Double -> Double -> Maybe (Double)
nearestRoot h disc a tMin tMax = let
    sqrtd = sqrt disc
    rootNeg = (h - sqrtd) / a;
    rootPos = (h + sqrtd) / a;
  in
    if (rootNeg <= tMin || tMax <= rootNeg)
      then if (rootPos <= tMin || tMax <= rootPos)
        then Nothing
        else Just rootNeg
      else Just rootPos

instance Hittable Sphere where
  hit (Sphere ctr rad) ray tMin tMax = let
      oc = ctr -^ origin ray;
      a = mag_squared (direction ray);
      h = (direction ray .^ oc);
      c = (mag_squared oc) - (rad * rad);
      discriminant = h*h - a*c;
    in
      if discriminant < 0
        then Nothing
        else
          case (nearestRoot h discriminant a tMin tMax) of
            Nothing -> Nothing
            Just root -> let pt = at ray root
              in Just HitRecord {
                point = pt,
                t = root,
                normal = (pt -^ ctr) /^ rad
              }
