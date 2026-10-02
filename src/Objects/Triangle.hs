module Objects.Triangle (Triangle(..), hitTriangle) where

import Vec3
import Point3
import Objects.HitRecord
import Objects.Material
import Ray (Ray(direction, origin), at)
import Interval

data Triangle = Triangle {
  vertexA :: Point3,
  vertexB :: Point3,
  vertexC :: Point3,
  material :: Material
}

hitTriangle :: Interval -> Ray -> Triangle -> Maybe HitRecord
hitTriangle (Interval tMin tMax) ray (Triangle a b c mat) =
  let
    edge1 = b -^ a
    edge2 = c -^ a
    rayCrossEdge2 = direction ray `cross` edge2
    determinant = edge1 .^ rayCrossEdge2
    epsilon = 0.00000001
  in
    if abs determinant < epsilon
      then Nothing
      else
        let
          inverseDeterminant = 1 / determinant
          s = origin ray -^ a
          u = inverseDeterminant * (s .^ rayCrossEdge2)
          sCrossEdge1 = s `cross` edge1
          v = inverseDeterminant * (direction ray .^ sCrossEdge1)
          rayT = inverseDeterminant * (edge2 .^ sCrossEdge1)
          outwardNormal = unit (edge1 `cross` edge2)
        in
          if u < 0 || u > 1 || v < 0 || u + v > 1 || rayT <= tMin || tMax <= rayT
            then Nothing
            else Just (createHitRecord ray (at ray rayT) outwardNormal rayT mat)
