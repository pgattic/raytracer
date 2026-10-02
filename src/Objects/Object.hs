module Objects.Object(Object(..), hit, HitRecord(..), createHitRecord) where

import Ray
import Objects.Sphere
import Objects.Triangle
import Objects.HitRecord
import Interval

data Object
  = SphereObj Sphere
  | TriangleObj Triangle

hit :: Interval -> Ray -> Object -> Maybe HitRecord
hit interval ray obj =
  case obj of
    SphereObj sphere -> hitSphere interval ray sphere
    TriangleObj triangle -> hitTriangle interval ray triangle
