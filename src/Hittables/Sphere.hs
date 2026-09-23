module Hittables.Sphere (Sphere(..), hitSphere) where

import Vec3
import Point3
import Hittables.HitRecord
import Hittables.Material
import Ray (Ray(direction, origin), at)
import Interval

data Sphere = Sphere {
  center :: Point3,
  radius :: Double,
  material :: Material
}

nearestRoot :: Double -> Double -> Double -> Interval -> Maybe (Double)
nearestRoot h disc a (Interval tMin tMax) = let
    sqrtd = sqrt disc
    rootNeg = (h - sqrtd) / a;
    rootPos = (h + sqrtd) / a;
  in
    if (rootNeg <= tMin || tMax <= rootNeg)
      then if (rootPos <= tMin || tMax <= rootPos)
        then Nothing
        else Just rootPos
      else Just rootNeg

hitSphere :: Interval -> Ray -> Sphere -> Maybe HitRecord
hitSphere interval ray (Sphere ctr rad mat) = let
    oc = ctr -^ origin ray;
    a = mag_squared (direction ray);
    h = (direction ray .^ oc);
    c = (mag_squared oc) - (rad * rad);
    discriminant = h*h - a*c;
  in
    if discriminant < 0
      then Nothing
      else
        case nearestRoot h discriminant a interval of
          Nothing -> Nothing
          Just rayT -> let pt = at ray rayT
            in Just (createHitRecord ray pt ((pt -^ ctr) /^ rad) rayT mat)
