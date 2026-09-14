module Hittables.Hittable(Hittable(..), hit, HitRecord(..), createHitRecord) where

import Ray
import Hittables.Sphere
import Hittables.HitRecord
import Interval
-- import Data.List (sortOn)
-- import Data.Maybe (catMaybes)

data Hittable = SphereObj Sphere

hit :: Interval -> Ray -> Hittable -> Maybe HitRecord
hit interval ray obj =
  case obj of
    SphereObj sphere -> hitSphere interval ray sphere

-- type Scene = [Hittable]
--
-- hitScene :: Scene -> Ray -> Double -> Double -> Maybe HitRecord
-- hitScene objects ray tMin tMax = let
--     hits = sortOn t (catMaybes (map (hit tMin tMax ray) objects));
--   in
--     case hits of
--       (h : _ ) -> Just h
--       _ -> Nothing
--

