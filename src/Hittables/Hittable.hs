module Hittables.Hittable(Hittable(..), hit, HitRecord(..), createHitRecord) where

import Ray
import Hittables.Sphere
import Hittables.HitRecord
import Interval

data Hittable = SphereObj Sphere

hit :: Interval -> Ray -> Hittable -> Maybe HitRecord
hit interval ray obj =
  case obj of
    SphereObj sphere -> hitSphere interval ray sphere
