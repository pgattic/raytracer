module Hittables.Sphere (Sphere(..), hit) where

import Vec3
import Point3
import Hittables.Hittable (Hittable(..), createHitRecord)
import Ray (Ray(direction, origin), at)

data Sphere = Sphere {
  center :: Point3,
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
        else Just rootPos
      else Just rootNeg

instance Hittable Sphere where
  hit tMin tMax ray (Sphere ctr rad) = let
      oc = ctr -^ origin ray;
      a = mag_squared (direction ray);
      h = (direction ray .^ oc);
      c = (mag_squared oc) - (rad * rad);
      discriminant = h*h - a*c;
    in
      if discriminant < 0
        then Nothing
        else
          case nearestRoot h discriminant a tMin tMax of
            Nothing -> Nothing
            Just rayT -> let pt = at ray rayT
              in Just (createHitRecord ray pt ((pt -^ ctr) /^ rad) rayT)
