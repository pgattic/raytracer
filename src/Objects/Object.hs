module Objects.Object(Object(..), hit, HitRecord(..), createHitRecord) where

import Ray
import Objects.Sphere
import Objects.HitRecord
import Interval

data Object = SphereObj Sphere

hit :: Interval -> Ray -> Object -> Maybe HitRecord
hit interval ray obj =
  case obj of
    SphereObj sphere -> hitSphere interval ray sphere
