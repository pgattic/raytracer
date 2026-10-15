module Objects.Object(Object(..), hit, buildBVH, HitRecord(..), createHitRecord) where

import Ray
import Objects.Sphere
import Objects.Triangle
import Objects.HitRecord
import Interval
import Bounds
import Vec3
import Data.List (sortOn)

data Object
  = SphereObj Sphere
  | TriangleObj Triangle
  | BVH Bounds Object Object

hit :: Interval -> Ray -> Object -> Maybe HitRecord
hit interval ray obj =
  case obj of
    SphereObj sphere -> hitSphere interval ray sphere
    TriangleObj triangle -> hitTriangle interval ray triangle
    BVH bounds left right
      | not (hitBounds interval ray bounds) -> Nothing
      | otherwise ->
          let first = hit interval ray left
              narrowed = Interval (minT interval) (maybe (maxT interval) t first)
          in case hit narrowed ray right of
               Nothing -> first
               closer -> closer

objectBounds :: Object -> Bounds
objectBounds (SphereObj (Sphere p radiusValue _)) =
  let r = abs radiusValue
  in Bounds (p -^ Vec3 r r r) (p +^ Vec3 r r r)
objectBounds (TriangleObj (Triangle a b c _)) =
  unionBounds (unionBounds (Bounds a a) (Bounds b b)) (Bounds c c)
objectBounds (BVH bounds _ _) = bounds

buildBVH :: [Object] -> [Object]
buildBVH [] = []
buildBVH objs = [build 0 objs]
  where
    build :: Int -> [Object] -> Object
    build _ [obj] = obj
    build axis items =
      let coordinate obj = case objectBounds obj of
            Bounds (Vec3 a b c) (Vec3 d e f) ->
              case axis of
                0 -> a+d
                1 -> b+e
                _ -> c+f
          sorted = sortOn coordinate items
          (ls,rs) = splitAt (length items `div` 2) sorted
          left = build ((axis+1) `mod` 3) ls
          right = build ((axis+1) `mod` 3) rs
      in BVH (unionBounds (objectBounds left) (objectBounds right)) left right
